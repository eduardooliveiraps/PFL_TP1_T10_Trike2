
main_menu :-
    clear_console,
    write_main_menu,
    read(Input),
    chosen_option(Input).

clear_console:- 
    write('\33\[2J').

write_main_menu :-
    write('*****************************************************************************'),nl,nl,
    write('1. Player vs Player'),nl,nl,
    write('2. Player vs Computer'),nl,nl,
    write('3. Computer vs Computer'),nl,nl,
    write('4. Instructions'),nl,nl,
    write('0. Exit'),nl,nl,
    write('*****************************************************************************'),nl,nl,nl,
    write('Enter one of the options (e.g. "1." for Player vs Player game mode): ').


chosen_option(0) :-
      clear_console,
      write('\nEnding the game. Thank you for playing Trike\n\n').

chosen_option(4) :-
    write('- The game board is an equilateral triangle'),nl,
    write('- A neutral pawn and black and white checkers are used to play.'),nl,
    write('- At the beginning of the game, the first player chooses a color and places a checker in any position on the board, with the neutral pawn on top of it.'),nl,
    write('- At this point, the second player has a unique chance to switch sides (change color)'),nl,
    write('- The neutral pawn can move any number of empty positions, in any direction in a straight line,'),nl,
    write('  but cannot move or jump over occupied positions.'),nl,
    write('- As soon as a player places a checker of his color in a permitted position, the pawn is placed on that same checker.'),nl,
    write('- When the pawn is trapped, the game ends.'),nl,
    write('- At the end of the game, each player earns one point for each checker of their color adjacent to or below the pawn.'),nl,
    read(_),
    main_menu.

chosen_option(_Other) :-
      write('\nERROR: Invalid option!\n\n'),
      write('Enter one of the options (e.g. "1." for Player vs Player game mode): '),
      read(Input),
      chosen_option(Input).
