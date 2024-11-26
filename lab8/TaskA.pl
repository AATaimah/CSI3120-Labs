:- dynamic destination/4.
:- dynamic expense/3.

% 1. Destination Management (Dynamic Predicates)
add_destination(Name, StartDate, EndDate, Budget) :-
    \+ destination(Name, _, _, _),
    assertz(destination(Name, StartDate, EndDate, Budget)).

remove_destination(Name) :-
    retract(destination(Name, _, _, _)),
    retractall(expense(Name, _, _)).

% 2. Expense Tracking
% Updated predicate for calculating total expenses using findall and sum_list
calculate_total_expenses(Destination, Total) :-
    findall(Amount, expense(Destination, _, Amount), Amounts),
    sum_list(Amounts, Total).

add_expense(Destination, Category, Amount) :-
    destination(Destination, _, _, Budget),
    assertz(expense(Destination, Category, Amount)),
    calculate_total_expenses(Destination, Total),
    (Total =< Budget -> true ; 
     format('Warning: Expenses ~w exceed budget ~w for ~w~n', [Total, Budget, Destination])).

% 3. Budget Validation
check_budget(Destination) :-
    destination(Destination, _, _, Budget),
    calculate_total_expenses(Destination, Total),
    format('Destination: ~w~n', [Destination]),
    format('Budget: ~w~n', [Budget]),
    format('Total Expenses: ~w~n', [Total]),
    (Total =< Budget 
     -> format('Budget Status: Within Limit~n') 
     ; format('Budget Status: Exceeded~n')).

% 4. Filtering Destinations and Expenses
% Date filtering with cut (!) to prevent backtracking
filter_destinations_by_date(StartDateStr, EndDateStr, FilteredDestinations) :-
    parse_date(StartDateStr, StartDate),
    parse_date(EndDateStr, EndDate),
    date_value(StartDate, StartValue),
    date_value(EndDate, EndValue),
    findall(Name, 
            (destination(Name, DestStartStr, DestEndStr, _),
             parse_date(DestStartStr, DestStart),
             parse_date(DestEndStr, DestEnd),
             date_value(DestStart, DestStartValue),
             date_value(DestEnd, DestEndValue),
             DestStartValue >= StartValue,
             DestEndValue =< EndValue),
            FilteredDestinations).


% Filter expenses by category with cut
filter_expenses_by_category(Destination, Category, FilteredExpenses) :-
    findall(Amount, 
            (expense(Destination, Category, Amount), !), 
            FilteredExpenses).

% Utility predicates for date parsing and comparison
parse_date(DateString, date(Year, Month, Day)) :-
    split_string(DateString, '-', '', [YearStr, MonthStr, DayStr]),
    number_string(Year, YearStr),
    number_string(Month, MonthStr),
    number_string(Day, DayStr).

date_value(date(Year,Month,Day), Value) :-
    Value is Year * 10000 + Month * 100 + Day.

% 5. Command Parsing with DCGs
% DCG for parsing commands
command(add_destination(Name, Start, End, Budget)) -->
    [add, destination], destination_details(Name, Start, End, Budget).

command(remove_destination(Name)) -->
    [remove, destination], [Name].

command(add_expense(Destination, Category, Amount)) -->
    [add, expense, for], [Destination], [Category], [Amount].

command(list_expenses(Destination)) -->
    [list, expenses, for], [Destination].

% Helper DCG for parsing destination details
destination_details(Name, Start, End, Budget) -->
    [Name], [Start], [End], [Budget].

% 6. File I/O with Error Handling
save_journey(Filename) :-
    catch(
        (open(Filename, write, Stream),
         findall(_, (destination(Name, Start, End, Budget), 
                     format(Stream, 'destination(\'~w\', \'~w\', \'~w\', ~w).~n', 
                            [Name, Start, End, Budget])), _),
         findall(_, (expense(Destination, Category, Amount), 
                     format(Stream, 'expense(\'~w\', \'~w\', ~w).~n', 
                            [Destination, Category, Amount])), _),
         close(Stream),
         format('Journey saved successfully to ~w~n', [Filename])),
        Error,
        handle_save_error(Error)
    ).

load_journey(Filename) :-
    catch(
        (retractall(destination(_, _, _, _)),
         retractall(expense(_, _, _)),
         consult(Filename),
         format('Journey loaded successfully from ~w~n', [Filename])),
        Error,
        handle_load_error(Error)
    ).

% Error handling predicates
handle_save_error(Error) :-
    format('Error saving journey: ~w~n', [Error]).

handle_load_error(Error) :-
    format('Error loading journey: ~w~n', [Error]).

% sum_list/2 predicate in case it's not available in your Prolog implementation
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.
