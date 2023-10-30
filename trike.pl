:- consult(board).
:- consult(menu).

play_game :-
    game_setup(GameState),
    display_game(GameState),
    game_cycle(GameState),
    clear_data.

game_cycle(GameState):-
    game_over(GameState, Winner), !,
    congratulate(Winner).
game_cycle(GameState):-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState), !,
    game_cycle(NewGameState).

display_game([Board,Player]) :-
    board(Cells,Board),
    board(Cells, Columns, Rows),
    display_line(Columns),
    display_rows(Board, Rows, Columns),
    display_column_numbering(1, Columns),nl,
    display_line(Columns), nl,
    display_player_turn(Player).

clear_data :-
    retractall(difficulty_level(_, _)),
    retractall(player(_, _)).
