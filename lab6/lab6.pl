

% Question 1
factorial(Input, _, 'Error: Input must be a non-negative integer.') :-
    ( \+ integer(Input) ; Input < 0 ), !.

factorial(0, TempResult, TempResult) :- !.

factorial(Input, TempResult, FinalResult) :-
    Input > 0,
    UpdatedTemp is TempResult * Input,
    UpdatedInput is Input - 1,
    factorial(UpdatedInput, UpdatedTemp, FinalResult).









% Question 2

% Define the filter_list/3 predicate
filter_list([], _, []).
filter_list([E|Es], Conditions, [E|Rest]) :-
    check_conditions(E, Conditions),
    filter_list(Es, Conditions, Rest).
filter_list([E|Es], Conditions, Rest) :-
    \+ check_conditions(E, Conditions),
    filter_list(Es, Conditions, Rest).

% Check if an element satisfies all conditions
check_conditions(_, []).
check_conditions(E, [Condition|RestConditions]) :-
    apply_condition(E, Condition),
    check_conditions(E, RestConditions).

% Apply a single condition to an element
apply_condition(E, Condition) :-
    Condition =.. [PredicateName, Value],
    NewCondition =.. [PredicateName, Value, E],
    call(NewCondition).

% Define the greater_than/2 predicate
greater_than(Value, Number) :-
    Number > Value.

% Define the multiple_of/2 predicate
multiple_of(Value, Number) :-
    Number mod Value =:= 0.






% Question 3

% second_max(+List, -SecondMax)
% Finds the second maximum distinct element in the List.
% If the list has fewer than two distinct elements, returns an error message.

second_max(List, SecondMax) :-
    % Remove duplicates and sort the list in ascending order
    sort(List, SortedSet),
    
    % Check if there are at least two distinct elements
    (   length(SortedSet, L),
        L >= 2
    ->  
        % Reverse the sorted list to have the maximum first
        reverse(SortedSet, [ _Max, SecondMax | _Rest ])
    ;   
        % Assign error message if not enough distinct elements
        SecondMax = 'Error: List must contain at least two distinct elements.'
    ).

% Helper predicate to reverse a list
% reverse(+List, -ReversedList)
reverse([], []).
reverse([Head|Tail], Reversed) :-
    reverse(Tail, RevTail),
    append(RevTail, [Head], Reversed).

