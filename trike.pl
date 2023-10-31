:- consult(board).
:- consult(menu).

%other_player(+CurrentPlayer,-NextPlayer)
other_player(p1,p2).
other_player(p2,p1).


play_game :-
    game_setup(GameState),
    display_game(GameState),
    game_cycle(GameState),
    clear_data.

game_cycle(GameState):-
    game_over(GameState, Winner), !,
    congratulate(Winner).
game_cycle(GameState):-
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState), !,
    game_cycle(NewGameState).

% display_game(-GameState)
% Displays the game board 
display_game([Board,Player,PlayNumber]) :-
    board(Cells,Board),
    board(Cells, Columns, Rows),
    display_line(Columns),
    display_column_numbering(1, Columns),nl,
    display_line(Columns),
    display_rows(Board, 1, Rows, Columns),
    display_line(Columns), nl,
    display_player_turn(Player).

% Eliminates predicates from the program's knowledge base
clear_data :-
    retractall(difficulty_level(_, _)),
    retractall(player(_, _)).

% choose_move(+GameState,-Move)
% A human player chooses a move
choose_move([Board,Player,PlayNumber], Row-Col) :-
    \+difficulty_level(Player, _),
    repeat,
    get_move([Board,Player,PlayNumber],Row-Col),
    validate_move([Board,Player,PlayNumber], Row-Col), !.
choose_move([Board,Player,PlayNumber], Move):-
    difficulty(Player, Level),                  
    choose_move([Board,Player,PlayNumber], Player, Level, Move), !. 

% get_move(+GameState,-Coordinate)
get_move([Board, Player, 2], Row-Col) :-
    swap_sides_decision(Choice),
    (Choice = 'y' ->
        write('You chose to swap sides!'), nl,
        swap_sides(Player),
        other_player(Player, NextPlayer),
        choose_move([Board, NextPlayer, 3], Row-Col)
    ;
        write('You chose not to swap sides. Proceed with the next move.'), nl,
        get_move([Board, Player, 3], Row-Col).
    ).  
get_move([Board, Player, PlayNumber], Row-Col):-
    board(Cells, Board),
    board(Cells, Columns, Rows),
    repeat,
    format('Select a row between 1 and ~d: ', [Rows]),
    read(TempRow),
    (
        valid_get_move_coordinate(TempRow, Rows) ->
            Row = TempRow
        ;
            write('Invalid row. Please choose a valid row.'), nl,
            fail
    ),
    repeat,
    format('Select a column between 1 and ~d: ', [Columns]),
    read(TempCol),
    (
        valid_get_move_coordinate(TempCol, Columns) ->
            Col = TempCol
        ;
            write('Invalid column. Please choose a valid column.'), nl,
            fail
    ).

% validate_move(+GameState,-Coordinate)
% Validates that the entered coordinates correspond to a valid position for inserting a checker
validate_move([Board, Player, 1], Row-Col) :-
    is_cell(Board, Row, Col).
validate_move([Board, Player, 1], Row-Col) :-
    \+ is_cell(Board, Row, Col),
    write('Invalid cell chosen. Please choose a valid cell.'), nl,
    get_move([Board, Player, 1], Row-Col).

validate_move([Board,Player,PlayNumber], Row-Col) :-


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Rules and Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valid_get_move_coordinate(+Value,-Max)
% Checks if the entered value is between 1 and the maximum
valid_get_move_coordinate(Value, Max) :-
    integer(Value),
    Value >= 1,
    Value =< Max.

% swap_sides_decision(+Choice)
% Ask the player if he wants to switch sides and save the answer
swap_sides_decision(Choice) :-
    write('Do you want to swap sides? [y or n]: '),
    read(Input),
    validate_swap_sides_choice(Input, Choice).
validate_swap_sides_choice('y', 'y') :- !.
validate_swap_sides_choice('Y', 'y') :- !.
validate_swap_sides_choice('N', 'n') :- !.
validate_swap_sides_choice('n', 'n') :- !.
validate_swap_sides_choice(_, Choice) :-
    write('Invalid input. Please enter y or n.'), nl,
    swap_sides_decision(Choice).

% swap_side(+CurrentPlayer)
% Swap the symbols that represent each player
swap_sides(Player) :-
    retractall(player_symbol(_,_)),
    asserta(player_symbol(p1,'O')),
    asserta(player_symbol(p2,'X')).

% is_cell(+Board, -Row, -Col)
% Checks whether the position with the respective coordinates is a cell
is_cell(Board, Row, Col) :-
    nth1(Row, Board, RowList), 
    nth1(Col, RowList, cell).































