:- use_module(library(random)).

% Dynamic predicates to track game state
:- dynamic 
    mine/2,           % Location of mines
    revealed/2,       % Revealed cells
    flagged/2,        % Flagged cells
    game_over/0.      % Flag to track game state

% Initialize the game board
initialize_game :-
    % Retract any existing game state
    retractall(mine(_,_)),
    retractall(revealed(_,_)),
    retractall(flagged(_,_)),
    retractall(game_over),
    
    % Use current time as seed for true randomness
    get_time(TimeStamp),
    Seed is round(TimeStamp * 1000),
    set_random(seed(Seed)),
    
    % Place 6 mines randomly
    place_mines(6).

% Place mines randomly on the grid
place_mines(0) :- !.
place_mines(N) :-
    random_between(1, 6, Row),
    random_between(1, 6, Col),
    (   \+ mine(Row, Col)
    ->  assertz(mine(Row, Col)),
        N1 is N - 1,
        place_mines(N1)
    ;   place_mines(N)
    ).

% Count neighboring mines
count_neighboring_mines(Row, Col, Count) :-
    findall(1, (
        neighbor_delta(DRow, DCol),
        NewRow is Row + DRow,
        NewCol is Col + DCol,
        valid_cell(NewRow, NewCol),
        mine(NewRow, NewCol)
    ), Mines),
    length(Mines, Count).

neighbor_delta(-1, -1).
neighbor_delta(-1, 0).
neighbor_delta(-1, 1).
neighbor_delta(0, -1).
neighbor_delta(0, 1).
neighbor_delta(1, -1).
neighbor_delta(1, 0).
neighbor_delta(1, 1).

% Check if cell is within bounds
valid_cell(Row, Col) :-
    between(1, 6, Row),
    between(1, 6, Col).

% Reveal a cell
reveal_cell(Row, Col) :-
    (   game_over
    ->  writeln('Game is over. Start a new game to continue playing.')
    ;   \+ valid_cell(Row, Col)
    ->  writeln('Invalid cell coordinates.')
    ;   revealed(Row, Col)
    ->  writeln('Cell is already revealed.')
    ;   flagged(Row, Col)
    ->  writeln('Cell is flagged. Unflag it before revealing.')
    ;   mine(Row, Col)
    ->  writeln('BOOM! You hit a mine. Game Over.'),
        reveal_original_mines,
        assertz(game_over)
    ;   % Safe to reveal
        assertz(revealed(Row, Col)),
        count_neighboring_mines(Row, Col, Count),
        (   Count = 0
        ->  reveal_zero_neighborhood(Row, Col)
        ;   true
        ),
        check_win_on_reveal
    ).

% Reveal neighborhood of zero-mine cells
reveal_zero_neighborhood(Row, Col) :-
    forall(
        (   neighbor_delta(DRow, DCol),
            NewRow is Row + DRow,
            NewCol is Col + DCol,
            valid_cell(NewRow, NewCol),
            \+ revealed(NewRow, NewCol),
            \+ flagged(NewRow, NewCol)
        ),
        reveal_cell(NewRow, NewCol)
    ).

% Flag a cell
flag_cell(Row, Col) :-
    (   game_over
    ->  writeln('Game is over. Start a new game to continue playing.')
    ;   \+ valid_cell(Row, Col)
    ->  writeln('Invalid cell coordinates.')
    ;   revealed(Row, Col)
    ->  writeln('Cannot flag a revealed cell.')
    ;   flagged(Row, Col)
    ->  writeln('Cell is already flagged.')
    ;   assertz(flagged(Row, Col)),
        check_win
    ).

% Unflag a cell
unflag_cell(Row, Col) :-
    (   game_over
    ->  writeln('Game is over. Start a new game to continue playing.')
    ;   \+ valid_cell(Row, Col)
    ->  writeln('Invalid cell coordinates.')
    ;   flagged(Row, Col)
    ->  retract(flagged(Row, Col))
    ;   writeln('Cell is not flagged.')
    ).

% Check if all mines are correctly flagged
check_win :-
    findall((Row, Col), mine(Row, Col), MineCells),
    findall((Row, Col), flagged(Row, Col), FlaggedCells),
    sort(MineCells, SortedMines),
    sort(FlaggedCells, SortedFlags),
    (   SortedMines == SortedFlags,
        \+ (member((R, C), FlaggedCells), \+ mine(R, C))
    ->  writeln('Congratulations! You flagged all mines and won the game!'),
        assertz(game_over)
    ;   true
    ).

% Check win condition after revealing a cell
check_win_on_reveal :-
    findall((Row, Col), (
        valid_cell(Row, Col),
        \+ mine(Row, Col)
    ), NonMineCells),
    findall((Row, Col), revealed(Row, Col), RevealedCells),
    sort(NonMineCells, SortedNonMines),
    sort(RevealedCells, SortedRevealed),
    (   SortedNonMines == SortedRevealed
    ->  writeln('Congratulations! You revealed all safe cells and won the game!'),
        assertz(game_over)
    ;   true
    ).

% Reveal original mines when game is over
reveal_original_mines :-
    forall(mine(Row, Col), assertz(revealed(Row, Col))).

% Display the game board
display_board :-
    writeln('  1 2 3 4 5 6'),
    display_rows(1).

display_rows(7) :- !.
display_rows(Row) :-
    write(Row),
    display_row_cells(Row, 1),
    NextRow is Row + 1,
    display_rows(NextRow).

display_row_cells(_, 7) :- nl, !.
display_row_cells(Row, Col) :-
    (   game_over
    ->  (   mine(Row, Col)
        ->  write(' X')
        ;   display_cell(Row, Col)
        )
    ;   display_cell(Row, Col)
    ),
    NextCol is Col + 1,
    display_row_cells(Row, NextCol).

display_cell(Row, Col) :-
    (   revealed(Row, Col)
    ->  (   mine(Row, Col)
        ->  write(' X')
        ;   count_neighboring_mines(Row, Col, Count),
            (   Count = 0
            ->  write('  ')
            ;   format(' ~w', [Count])
            )
        )
    ;   flagged(Row, Col)
    ->  write(' F')
    ;   write(' .')
    ).

% Start game
play :-
    initialize_game,
    writeln('Minesweeper Game Started!'),
    writeln('Commands:'),
    writeln('reveal(Row, Col). - Reveal a cell'),
    writeln('flag(Row, Col).   - Flag a cell'),
    writeln('unflag(Row, Col). - Unflag a cell'),
    writeln('board.            - Display current board'),
    display_board.

% Debug predicates
print_mines :-
    findall((Row, Col), mine(Row, Col), Mines),
    writeln('Mine Locations:'),
    print_mine_list(Mines).

print_mine_list([]).
print_mine_list([(Row, Col)|Rest]) :-
    format('Mine at (Row: ~w, Col: ~w)~n', [Row, Col]),
    print_mine_list(Rest).

% Convenience predicates for gameplay
reveal(Row, Col) :-
    reveal_cell(Row, Col),
    display_board.

flag(Row, Col) :-
    flag_cell(Row, Col),
    display_board.

unflag(Row, Col) :-
    unflag_cell(Row, Col),
    display_board.

board :-
    display_board.
