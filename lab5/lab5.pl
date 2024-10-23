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
