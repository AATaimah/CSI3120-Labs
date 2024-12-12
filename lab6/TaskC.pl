is_prime(N) :-
    N > 1,
    is_prime_helper(N, 2).

is_prime_helper(N, Divisor) :-
    Divisor * Divisor > N, !.
is_prime_helper(N, Divisor) :-
    N mod Divisor =\= 0,
    NextDivisor is Divisor + 1,
    is_prime_helper(N, NextDivisor).

reverse_number(N, Reversed) :-
    number_chars(N, Chars),
    reverse(Chars, ReversedChars),
    number_chars(Reversed, ReversedChars).

filter_and_transform(Input, Result) :-
    filter_and_transform_helper(Input, [], Result, 0).

filter_and_transform_helper([], Acc, Result, _) :-
    reverse(Acc, Result).
filter_and_transform_helper(_, Acc, Result, 5) :-
    reverse(Acc, Result), !.
filter_and_transform_helper([H|T], Acc, Result, Count) :-
    (is_prime(H) ->
        reverse_number(H, ReversedH),
        NewCount is Count + 1,
        filter_and_transform_helper(T, [ReversedH|Acc], Result, NewCount)
    ;
        filter_and_transform_helper(T, Acc, Result, Count)
    ).