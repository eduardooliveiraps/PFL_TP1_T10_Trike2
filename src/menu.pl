:- consult(board).

% valid_board_size(+Size)
% Facts to validate the board size
valid_board_size(6).
valid_board_size(10).
valid_board_size(15).

% valid_computer_level(+Level)
% Facts to validate the computer's difficulty levels
valid_computer_level(1).
valid_computer_level(2).

% game_setup(-GameState)
% Prompts the user to select a game mode, handles the chosen mode, chooses the player who makes the first move,
% asks for the board size, and initializes the board state.
% Initializes the game state with the Board and the player who makes the first move
game_setup([Board, Player, MoveNumber]) :-
    write_main_menu,
    read(Input),
    menu_option(Input),
    first_move_player(Player),
    board_size(Size),
    default_player_checker,
    set_default_neutral_pawn_coordinates(Size),
    MoveNumber is 1,
    initial_state(Size, [Board,_,_]), !.

% Main menu and its options
write_main_menu :-
    write('*****************************************************************************'),nl,nl,
    write('1. Human vs Human'),nl,nl,
    write('2. Human vs Computer'),nl,nl,
    write('3. Computer vs Computer'),nl,nl,
    write('0. Exit'),nl,nl,
    write('*****************************************************************************'),nl,nl,nl,
    write('Enter one of the options (e.g. "1." for Human vs Human game mode): ').

% menu_option(+Input)
% Based on the user input, executes a specific action corresponding to the chosen game mode or handles an invalid input.
menu_option(0) :-
    write('\nEnding the game. Thank you for playing Trike\n\n').

menu_option(1) :-
    write('Human vs Human\n'),
    asserta(player(p1, player1)),
    asserta(player(p2, player2)).

menu_option(2) :-
    write('Human vs Computer\n'),
    asserta(player(p1, player)),
    asserta(player(p2, computer)),
    computer_difficulty_level(p2).

menu_option(3) :-
    write('Computer vs Computer\n'),
    asserta(player(p1, computer1)),
    asserta(player(p2, computer2)),
    computer_difficulty_level(p1),
    computer_difficulty_level(p2).

menu_option(_Other) :-
    write('\nERROR: Invalid option!\n\n'),
    write('Enter one of the options (e.g. "1." for Player vs Player game mode): '),
    read(Input),
    menu_option(Input).

% first_move_player(-Player)
% Selects randomly which player makes the first move.
first_move_player(Player) :-
    write('The choice of the starting player is random.\n'),
    player(p1, Player1),
    player(p2, Player2),
    random_member(FirstPlayer, [Player1, Player2]),
    format('The player who will make the first move is: ~w\n', [FirstPlayer]),
    player(P, FirstPlayer),
    Player = P.

% board_size(-Size)
% Reads the chosen size for the game board from the user input.
board_size(Size) :-
    repeat,
    write('Choose one of the possible sizes for the game board: 6, 10, or 15: '),
    read(TempSize),
    (   
        valid_board_size(TempSize) ->
            Size = TempSize
        ;   
            write('Invalid board size. Please choose a valid size.'), nl,
            fail
    ).
% computer_difficulty_level(+Computer)
% Enables the user to choose the difficulty level for the specified Computer
computer_difficulty_level(Computer) :-
    repeat,
    format('Select one of the difficulty levels for the ~a:\n', [Computer]),
    write('Level 1 - Random Movement\n'),
    write('Level 2 - Greedy Movement\n'),
    read(Level),
    (   
        valid_computer_level(Level) ->
            asserta(difficulty_level(Computer, Level))
        ;   
            write('Invalid computer level. Please choose a valid computer level ("1." or "2.").'), nl,
            fail
    ).

% Introduces information relating to the symbol that represents each player into the program's knowledge base
default_player_checker :-
    asserta(player_checker(p1, x)),
    asserta(player_checker(p2, o)).

set_default_neutral_pawn_coordinates(Size) :-
    board(Size, Cols, Rows),
    asserta(neutral_pawn_coordinates(Rows-Cols)).