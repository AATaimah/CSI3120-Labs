/* Task 1 */

/* Parent-child relationships */
parent(john, mary).
parent(john, tom).
parent(mary, ann).
parent(mary, fred).
parent(tom, liz).

/* Genders */
male(john).
male(tom).
male(fred).
female(mary).
female(ann).
female(liz).

/* Check if siblings */
sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y.

/* Check if grandparent */
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).

/* Check if ancestor */
ancestor(Anc, Desc) :-
    parent(Anc, Desc).

ancestor(Anc, Desc) :-
    parent(Anc, P),
    ancestor(P, Desc).




/* Task 2 */
% Base case: an empty list has a sum of 0
sum_odd_numbers([], 0).

% Recursive case: if the head is odd, add it to the sum of the tail
sum_odd_numbers([Head|Tail], Sum) :-
    Head mod 2 =:= 1,  % Check if Head is odd
    sum_odd_numbers(Tail, TailSum),  % Recursively calculate sum of tail
    Sum is Head + TailSum.

% Recursive case: if the head is even, skip it and process the tail
sum_odd_numbers([Head|Tail], Sum) :-
    Head mod 2 =:= 0,  % Check if Head is even
    sum_odd_numbers(Tail, Sum).  % Continue with tail


/* Task 3 */


solve_puzzle(Houses, GreenIndex) :-
    % Hint: Use the built-in Prolog Predicate to define all the possible combinations of the houses.
    permutation([red, blue, green, yellow], Houses),
    
    % Clue 1: The red house is immediately to the left of the blue house
    immediately_left_of(red, blue, Houses),
    
    % Clue 2: The treasure is in the green house
    nth1(GreenIndex, Houses, green),
    
    % Clue 3: The yellow house is not next to the green house
    not_next_to(yellow, green, Houses),
    
    % Clue 4: The green house is not in the second position from the left
    GreenIndex \= 2.

% Helper predicate to check if one element is immediately to the left of another
immediately_left_of(Left, Right, List) :-
    append(_, [Left, Right | _], List).

% Helper predicate to check if two elements are next to each other
next_to(X, Y, List) :-
    append(_, [X, Y | _], List);
    append(_, [Y, X | _], List).

% Helper predicate to check if two elements are not next to each other
not_next_to(X, Y, List) :-
    \+ next_to(X, Y, List).
