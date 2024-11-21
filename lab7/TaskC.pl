:- dynamic book/4.
:- dynamic borrowed/4.

% Initialize the library
initialize_library :-
    retractall(book(_, _, _, _)),
    retractall(borrowed(_, _, _, _)).

% Add a book to the library
add_book(Title, Author, Year, Genre) :-
    \+ book(Title, Author, Year, Genre), % Prevent duplicates
    assertz(book(Title, Author, Year, Genre)).

% Remove a book from the library
remove_book(Title, Author, Year, Genre) :-
    retract(book(Title, Author, Year, Genre)),
    % Also remove any borrowing records for this book
    (borrowed(Title, Author, Year, Genre) -> 
        retract(borrowed(Title, Author, Year, Genre)) ; 
        true).

% Check book availability
is_available(Title, Author, Year, Genre) :-
    book(Title, Author, Year, Genre),
    \+ borrowed(Title, Author, Year, Genre).

% Borrow a book
borrow_book(Title, Author, Year, Genre) :-
    is_available(Title, Author, Year, Genre),
    assertz(borrowed(Title, Author, Year, Genre)).

% Return a book
return_book(Title, Author, Year, Genre) :-
    retract(borrowed(Title, Author, Year, Genre)).

% Find books by author
find_by_author(Author, Books) :-
    findall(Title, book(Title, Author, _, _), Books).

% Find books by genre
find_by_genre(Genre, Books) :-
    findall(Title, book(Title, _, _, Genre), Books).

% Find books by year
find_by_year(Year, Books) :-
    findall(Title, book(Title, _, Year, _), Books).

% Recommend books by genre
recommend_by_genre(Genre, Recommendations) :-
    findall(Title, book(Title, _, _, Genre), Recommendations).

% Recommend books by author
recommend_by_author(Author, Recommendations) :-
    findall(Title, book(Title, Author, _, _), Recommendations).

% Helper predicate to list all books
list_books :-
    findall((Title, Author, Year, Genre), book(Title, Author, Year, Genre), Books),
    print_books(Books).

% Print books
print_books([]).
print_books([(Title, Author, Year, Genre)|Rest]) :-
    format('Title: ~w, Author: ~w, Year: ~d, Genre: ~w~n', 
           [Title, Author, Year, Genre]),
    print_books(Rest).

