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
