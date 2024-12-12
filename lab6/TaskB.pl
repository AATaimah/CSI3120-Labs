% Helper predicate to check if age is in the specified range
age_in_range(MinAge, MaxAge, Age) :-
    Age >= MinAge,
    Age < MaxAge.

% Main taller_than predicate with cut operator
taller_than([], _, _, 'No match found.') :- !.
taller_than([Person|_], Height, AgeRange, Person) :-
    Person = person(_, PersonHeight, PersonAge),
    PersonHeight > Height,
    call(AgeRange, PersonAge),
    !.  

taller_than([_|Rest], Height, AgeRange, Result) :-
    taller_than(Rest, Height, AgeRange, Result).