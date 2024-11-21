% Question 1: Right-angle triangle with error handling
right_angle_triangle_console :-
    write('Enter the height of the right-angled triangle: '),
    read(Height),
    (   integer(Height), Height > 0 ->
        nl,
        print_triangle(1, Height)
    ;   
        write('Error: Height must be a positive integer.'),
        nl,
        fail
    ).

% Base case: Stop when current row exceeds height
print_triangle(Current, Height) :-
    Current > Height, !.

% Recursive case: Print current row and move to next
print_triangle(Current, Height) :-
    print_row(1, Current),
    nl,
    NextRow is Current + 1,
    print_triangle(NextRow, Height).

% Print a single row of the triangle
print_row(Count, Max) :-
    Count > Max, !.
print_row(Count, Max) :-
    write('#'),
    NextCount is Count + 1,
    print_row(NextCount, Max).


    

% Question 2: Isosceles triangle pattern with error handling
isosceles_triangle_pattern_file(Height, Filename) :-
    (   integer(Height), Height > 0 ->
        open(Filename, write, Stream),
        write_triangle_pattern(1, Height, Stream),
        close(Stream),
        format('Isosceles triangle pattern written to file: ~w~n', [Filename])
    ;
        format('Error: Height ~w must be a positive integer.', [Height]),
        nl,
        fail
    ).

% Helper predicate to write each line of the triangle
write_triangle_pattern(CurrentRow, Height, Stream) :-
    CurrentRow =< Height,
    Spaces is Height - CurrentRow,
    Stars is 2 * CurrentRow - 1,
    write_spaces(Spaces, Stream),
    write_stars(Stars, Stream),
    nl(Stream),
    NextRow is CurrentRow + 1,
    write_triangle_pattern(NextRow, Height, Stream).
write_triangle_pattern(CurrentRow, Height, _) :-
    CurrentRow > Height.

% Predicate to write a specific number of spaces
write_spaces(0, _) :- !.
write_spaces(N, Stream) :-
    N > 0,
    write(Stream, ' '),
    N1 is N - 1,
    write_spaces(N1, Stream).

% Predicate to write a specific number of stars
write_stars(0, _) :- !.
write_stars(N, Stream) :-
    N > 0,
    write(Stream, '*'),
    N1 is N - 1,
    write_stars(N1, Stream).