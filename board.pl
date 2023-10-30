
% board(+Size, -Board)
% Defines the initial game board for different sizes.
board(6,[
    [blank, blank,  cell, blank, blank],
    [blank,  cell, blank,  cell, blank],
    [ cell, blank,  cell, blank,  cell]
]).

board(10,[
    [blank, blank, blank,  cell, blank, blank, blank],
    [blank, blank,  cell, blank,  cell, blank, blank],
    [blank,  cell, blank,  cell, blank,  cell, blank], 
    [ cell, blank,  cell, blank,  cell, blank,  cell]   
]).

board(15,[
    [blank, blank, blank, blank,  cell, blank, blank, blank, blank],
    [blank, blank, blank,  cell, blank,  cell, blank, blank, blank],
    [blank, blank,  cell, blank,  cell, blank,  cell, blank, blank],
    [blank,  cell, blank,  cell, blank,  cell, blank,  cell, blank],
    [ cell, blank,  cell, blank,  cell, blank,  cell, blank,  cell]
]).

% board(+Cells, +Columns, +Rows)
board(6,5,3).
board(10,7,4).
board(15,9,5).

% symbol(+Element, -Symbol)
symbol(p1, S) :- S='1'.     
symbol(p2, S) :- S='2'.
symbol(w,S) :- S='W'.
symbol(b,S) :- S='B'.
symbol(n,S) :- S='N'.
symbol(blank,S) :- S='---'.
symbol(cell,S) :- S='   '.

% initial_state(+Size, -GameState)
% Initializes the initial state of the game based on the given board size.
initial_state(Size,[Board,Player]) :-
    board(Size,Board),
    GameState = [Board,Player].

% display_column_numbering(+ColumnNumber, +TotalNumberOfColumns)
% Displays column numbering from 1 to Max on the game board.
display_column_numbering(Max, Max):-
    format('| ~d |', [Max]), !.
display_column_numbering(1, Max):-
    write('   | 1 '),
    display_column_numbering(2, Max), !.
display_column_numbering(N, Max):-
    N > 9,
    format('| ~d ', [N]),
    Next is N + 1,
    display_column_numbering(Next, Max).
display_column_numbering(N, Max):-
    format('| ~d ', [N]),
    Next is N + 1,
    display_column_numbering(Next, Max).

% display_line(+TotalNumberOfColumns)
% Generates and prints a horizontal line separator for the game board.
display_line(0):-
    write('---|\n'), !.
display_line(N):-
    write('---|'),
    N1 is N - 1,
    display_line(N1).

% display_rows(+Board, +Rows, +Columns)
% Displays the rows of the game board with their respective elements.
display_rows([], 0, _).
display_rows([Line|Rest], Rows, Columns) :-
    format(' ~d |', [Rows]),
    display_elements(Line),nl,
    display_line(Columns),
    RemainingRows is Rows - 1,
    display_rows(Rest, RemainingRows,  Columns).

% display_elements(+Line)
% Displays the elements of a particular row in the game board.
display_elements([]).
display_elements([CurrentElement|Rest]) :-
    symbol(CurrentElement, Symbol),
    format('~w|', [Symbol]),
    display_elements(Rest).

% display_player_turn(+Player)
% Displays a message indicating the player who will make the next move
display_player_turn(Player) :-
    symbol(Player, Symbol),
    format(' > Player ~w turn to play!', [Symbol]).

