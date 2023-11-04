:- use_module(library(lists)).
:- use_module(library(random)).

% dynamic player(+Player,-PlayerType)
:- dynamic player/2.

% dynamic difficulty(+Computer,-Level)
:- dynamic difficulty_level/2.

% dynamic player_checker(+Player,-Symbol)
:- dynamic player_checker/2.

% dynamic neutral_pawn_coordinates(Row-Col)
:- dynamic neutral_pawn_coordinates/1.

% dynamic player_score(+Player, -Score)
:- dynamic player_score/2.

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
symbol(x,S) :- S=' X '. 
symbol(o,S) :- S=' O '.
symbol(n,S) :- S=' N '.
symbol(blank,S) :- S='---'.
symbol(cell,S) :- S='   '.

% initial_state(+Size, -GameState)
% Initializes the initial state of the game based on the given board size.
initial_state(Size,[Board,_,_]) :-
    board(Size,Board).

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

% display_rows(+Board, +Row, +TotalRows, +Columns)
% Displays the rows of the game board with their respective elements.
display_rows(_, LineNumber, Rows, _):- 
    LineNumber > Rows, nl, !.
display_rows([Line|Rest], LineNumber, Rows, Columns):-
    LineNumber > 9,
    format(' ~d |', [LineNumber]),
    display_elements(Line),nl,
    display_line(Columns),
    NextLineNumber is LineNumber + 1,
    display_rows(Rest, NextLineNumber, Rows, Columns), !.
display_rows([Line|Rest], LineNumber, Rows, Columns):-
    format(' ~d |', [LineNumber]),
    display_elements(Line),nl,
    display_line(Columns),
    NextLineNumber is LineNumber + 1,
    display_rows(Rest, NextLineNumber, Rows, Columns).

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
    player(Player, PlayerType),
    player_checker(Player, Checker),
    symbol(Checker, Symbol),
    format(' > ~w turn to play! Your checker is:~w', [PlayerType, Symbol]).


% direction_from_checker(+BoardSize, +NeutralPawnCoordinate, +PathCellList)
%Predicate that presents a list with coordinates of cells that can be reached
% in a given direction from a predefined cell, on a fixed size board

% Board Size 6
direction_from_checker(6, 1-3, [2-4, 3-5]).
direction_from_checker(6, 1-3, [2-2, 3-1]).

direction_from_checker(6, 2-2, [1-3]).
direction_from_checker(6, 2-2, [3-1]).
direction_from_checker(6, 2-2, [3-3]).
direction_from_checker(6, 2-2, [2-4]).

direction_from_checker(6, 2-4, [1-3]).
direction_from_checker(6, 2-4, [2-2]).
direction_from_checker(6, 2-4, [3-3]).
direction_from_checker(6, 2-4, [3-5]).

direction_from_checker(6, 3-1, [2-2, 1-3]).
direction_from_checker(6, 3-1, [3-3, 3-5]).

direction_from_checker(6, 3-3, [3-1]).
direction_from_checker(6, 3-3, [2-2]).
direction_from_checker(6, 3-3, [2-4]).
direction_from_checker(6, 3-3, [3-5]).

direction_from_checker(6, 3-5, [2-4, 1-3]).
direction_from_checker(6, 3-5, [3-3, 3-1]).

% Board Size 10
direction_from_checker(10, 1-4, [2-3, 3-2, 4-1]).
direction_from_checker(10, 1-4, [2-5, 3-6, 4-7]).

direction_from_checker(10, 2-3, [1-4]).
direction_from_checker(10, 2-3, [2-5]).
direction_from_checker(10, 2-3, [3-4, 4-5]).
direction_from_checker(10, 2-3, [3-2, 4-1]).

direction_from_checker(10, 2-5, [1-4]).
direction_from_checker(10, 2-5, [2-3]).
direction_from_checker(10, 2-5, [3-4, 4-3]).
direction_from_checker(10, 2-5, [3-6, 4-7]).

direction_from_checker(10, 3-2, [2-3, 1-4]).
direction_from_checker(10, 3-2, [3-4, 3-6]).
direction_from_checker(10, 3-2, [4-3]).
direction_from_checker(10, 3-2, [4-1]).

direction_from_checker(10, 3-4, [2-3]).
direction_from_checker(10, 3-4, [2-5]).
direction_from_checker(10, 3-4, [3-2]).
direction_from_checker(10, 3-4, [3-6]).
direction_from_checker(10, 3-4, [4-3]).
direction_from_checker(10, 3-4, [4-5]).

direction_from_checker(10, 3-6, [2-5, 1-4]).
direction_from_checker(10, 3-6, [3-4, 3-2]).
direction_from_checker(10, 3-6, [4-5]).
direction_from_checker(10, 3-6, [4-7]).

direction_from_checker(10, 4-1, [3-2, 2-3, 1-4]).
direction_from_checker(10, 4-1, [4-3, 4-5, 4-7]).

direction_from_checker(10, 4-3, [4-1]).
direction_from_checker(10, 4-3, [3-2]).
direction_from_checker(10, 4-3, [3-4, 2-5]).
direction_from_checker(10, 4-3, [4-5, 4-7]).

direction_from_checker(10, 4-5, [4-7]).
direction_from_checker(10, 4-5, [3-6]).
direction_from_checker(10, 4-5, [3-4, 2-3]).
direction_from_checker(10, 4-5, [4-3, 4-1]).

direction_from_checker(10, 4-7, [4-5, 4-3, 4-1]).
direction_from_checker(10, 4-7, [3-6, 2-5, 1-4]).

% Board Size 15
direction_from_checker(15, 1-5, [2-4, 3-3, 4-2, 5-1]).
direction_from_checker(15, 1-5, [2-6, 3-7, 4-8, 5-9]).

direction_from_checker(15, 2-4, [1-5]).
direction_from_checker(15, 2-4, [2-6]).
direction_from_checker(15, 2-4, [3-5, 4-6, 5-7]).
direction_from_checker(15, 2-4, [3-3, 4-2, 5-1]).

direction_from_checker(15, 2-6, [1-5]).
direction_from_checker(15, 2-6, [2-4]).
direction_from_checker(15, 2-6, [3-5, 4-4, 5-3]).
direction_from_checker(15, 2-6, [3-7, 4-8, 5-9]).

direction_from_checker(15, 3-3, [2-4, 1-5]).
direction_from_checker(15, 3-3, [3-5, 3-7]).
direction_from_checker(15, 3-3, [4-4, 5-5]).
direction_from_checker(15, 3-3, [4-2, 5-1]).

direction_from_checker(15, 3-5, [2-4]).
direction_from_checker(15, 3-5, [2-6]).
direction_from_checker(15, 3-5, [3-3]).
direction_from_checker(15, 3-5, [3-7]).
direction_from_checker(15, 3-5, [4-4, 5-3]).
direction_from_checker(15, 3-5, [4-6, 5-7]).

direction_from_checker(15, 3-7, [2-6, 1-5]).
direction_from_checker(15, 3-7, [3-5, 3-3]).
direction_from_checker(15, 3-7, [4-6, 5-5]).
direction_from_checker(15, 3-7, [4-6, 5-9]).

direction_from_checker(15, 4-2, [3-3, 2-4, 1-5]).
direction_from_checker(15, 4-2, [4-4, 4-6, 4-8]).
direction_from_checker(15, 4-2, [5-3]).
direction_from_checker(15, 4-2, [5-1]).

direction_from_checker(15, 4-4, [3-3]).
direction_from_checker(15, 4-4, [3-5, 2-6]).
direction_from_checker(15, 4-4, [4-2]).
direction_from_checker(15, 4-4, [4-6, 4-8]).
direction_from_checker(15, 4-4, [5-3]).
direction_from_checker(15, 4-4, [5-5]).

direction_from_checker(15, 4-6, [3-5, 2-4]).
direction_from_checker(15, 4-6, [3-7]).
direction_from_checker(15, 4-6, [4-4, 4-2]).
direction_from_checker(15, 4-6, [4-8]).
direction_from_checker(15, 4-6, [5-5]).
direction_from_checker(15, 4-6, [5-7]).

direction_from_checker(15, 4-8, [3-7, 2-6, 1-5]).
direction_from_checker(15, 4-8, [4-6, 4-4, 4-2]).
direction_from_checker(15, 4-8, [5-7]).
direction_from_checker(15, 4-8, [5-9]).

direction_from_checker(15, 5-1, [4-2, 3-3, 2-4, 1-5]).
direction_from_checker(15, 5-1, [5-3, 5-5, 5-7, 5-9]).

direction_from_checker(15, 5-3, [5-1]).
direction_from_checker(15, 5-3, [4-2]).
direction_from_checker(15, 5-3, [4-4, 3-5, 2-6]).
direction_from_checker(15, 5-3, [5-5, 5-7, 5-9]).

direction_from_checker(15, 5-5, [5-3, 5-1]).
direction_from_checker(15, 5-5, [4-4, 3-3]).
direction_from_checker(15, 5-5, [4-6, 3-7]).
direction_from_checker(15, 5-5, [5-7, 5-9]).

direction_from_checker(15, 5-7, [5-5, 5-3, 5-1]).
direction_from_checker(15, 5-7, [4-6, 3-5, 2-4]).
direction_from_checker(15, 5-7, [4-8]).
direction_from_checker(15, 5-7, [5-9]).

direction_from_checker(15, 5-9, [4-8, 3-7, 2-6, 1-5]).
direction_from_checker(15, 5-9, [5-7, 5-5, 5-3, 5-1]).

























































