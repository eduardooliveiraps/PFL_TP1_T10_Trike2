# TRIKE
Grupo Trike_2:

| Nome                         | UP            | Contribuição |
| ------------                 | ------------  |------------  |
| Bruno Miguel de Siuéia Duvane| [up202109244] |50%           |
| Eduardo Martins Oliveira     | [up202108690] |50%           |


## Instalação e Execução

Para instalar o jogo Trike primeiro é necessário fazer o download dos ficheiros presentes em PFL_TP1_T10_Trike2.zip e descompactá-los. Dentro do diretório src consulte o ficheiro trike.pl através da linha de comandos ou pela própria UI do Sicstus Prolog 4.8.
O jogo inicia-se com o predicado play/0

```prolog
?- play.
```

## Descrição do Jogo

Trike é um jogo de estratégia abstrata combinatória para dois jogadores. O jogo gira em torno de criar armadilhas, desmontar armadilhas do adversário e manobrar uma peça compartilhada (peão neutro) para a sua própria armadilha com o objetivo de vencer.
  
Regras: 
- O tabuleiro do jogo tem a forma de um triângulo equilátero.
- Um peão neutro e peças pretas e brancas são usadas para jogar.
- No início do jogo, o primeiro jogador coloca uma peça em qualquer posição do tabuleiro, com o peão neutro em cima dela. Somente neste momento, o segundo jogador tem uma chance única de trocar de lado (mudar de peça) em vez de fazer um movimento normal.
- O peão neutro pode mover qualquer número de posições vazias, em qualquer direção em linha reta, mas não pode mover-se ou saltar sobre as posições ocupadas.
- Assim que um jogador coloca uma peça numa posição permitida, o peão neutro fica sobre essa mesma peça.
- Quando o peão neutro fica preso, o jogo termina.
- No final do jogo, cada jogador ganha um ponto para cada peça da sua cor adjacente ou abaixo do peão neutro. A pessoa com a maior pontuação vence. Não existem empates.
    
Nota: Esta informação encontra-se detalhada no [website oficial do jogo](https://boardgamegeek.com/boardgame/307379/trike)

## **Lógica do Jogo**

### **Representação Interna do Estado do Jogo**
Em qualquer momento do jogo, o estado do jogo (GameState) é representado por uma lista de três elementos: [Board,Player,MoveNumber], onde Board é o estado atual do tabuleiro, Player é o jogador atual que realizará a próxima jogada e MoveNumber é o número da jogada.
O Player é representado por um átomo que pode ser "p1" para o player 1 ou "p2" para o player 2, dependendo do jogador atual. Cada célula do tabuleiro pode ser representada pelo átomo "n" para o peão neutro, "x" para uma peça branca, "o" para uma peça preta, "blank" para uma posição não jogável, "cell" para uma posição vazia. 

### **`GameState` durante um jogo**
#### **Estado Inicial - tabuleiro com 6 posições**
```prolog
[ [[blank, blank,  cell, blank, blank],
   [blank,  cell, blank,  cell, blank],
   [ cell, blank,  cell, blank,  cell]],
  Player,
  1
]
```
```
---|---|---|---|---|---|
   | 1 | 2 | 3 | 4 | 5 |
---|---|---|---|---|---|
 1 |---|---|   |---|---|
---|---|---|---|---|---|
 2 |---|   |---|   |---|
---|---|---|---|---|---|
 3 |   |---|   |---|   |
---|---|---|---|---|---|
```
#### **Estado Intermédio - tabuleiro com 6 posições**
```prolog
[ [[blank, blank,  cell, blank, blank],
   [blank,     o, blank,     x, blank],
   [ cell, blank,     n, blank,  cell]],
  Player,
  MoveNumber,
]
```
```
---|---|---|---|---|---|
   | 1 | 2 | 3 | 4 | 5 |
---|---|---|---|---|---|
 1 |---|---|   |---|---|
---|---|---|---|---|---|
 2 |---| O |---| X |---|
---|---|---|---|---|---|
 3 |   |---| N |---|   |
---|---|---|---|---|---|
```
#### **Estado Final - tabuleiro com 6 posições**
```prolog
[ [[blank, blank,  cell, blank, blank],
   [blank,     o, blank,     x, blank],
   [ cell, blank,     x, blank,    n]],
  Player,
  MoveNumber,
]
```
```
---|---|---|---|---|---|
   | 1 | 2 | 3 | 4 | 5 |
---|---|---|---|---|---|
 1 |---|---|   |---|---|
---|---|---|---|---|---|
 2 |---| O |---| X |---|
---|---|---|---|---|---|
 3 |   |---| X |---| N |
---|---|---|---|---|---|
```

### **Visualização do Estado do Jogo**
Antes de iniciar o jogo, o utilizador é convidados a configurar a partida. O utilizador pode decidir o modo de jogo e o tamanho do tabuleiro. A decisão do jogador que fará a primeira jogada é calculada de forma aleatória.

Após a escolha do tamanho do tabuleiro o próprio é inicializado com initial_state(+Size, -GameState).

Depois da configuração da partida, o predicado `display_game(+GameState)` é responsável por mostrar o tabuleiro e o jogador que fará a próxima jogada.

### **Validação e Execução do Movimento**

O jogo funciona com base num ciclo cujo único caso de paragem é a vitória de um dos jogadores.

Em choose_move/2 o jogador é convidado a inserir duas coordenadas. As coordenadas resultantes são avaliadas através do predicado validate_move/2. Considera-se um movimento válido se as coordenadas inseridas corresponderem às coordenadas de uma posição vazia, se a posição está numa direção em linha reta em relação à posição do peão neutro, e se esse caminho não está obstruído, ou seja, as posições entre o peão neutro e a posição inserida estão desocupadas.

Após selecionar corretamente o movimento, o predicado move/3 é responável por colocar o peão neutro na posição inserida e por colocar uma peça do jogador que realizou a jogada anterior na posição anterior do peão neutro.

### **List of valid moves**

The `valid_moves(+GameState, -ListOfMoves)` predicate takes two arguments: the current game state and a list of valid moves. The function is defined twice, once for each player (p1 and p2).

For player p1, the function begins by calling the `findall` predicate to find all the red stones on the board (represented as a list of column-row pairs). It then calls the `find_moves` predicate to determine the possible moves (jump up, jump down and move sideways) that can be made with the jumpers, and it assigns the resulting list of moves to the `List1` variable. 
Next, it calls `findall` again to find all the slipper stones belonging to player p1 and calls the `find_normal_move` predicate to determine the possible moves (sideways) that can be made with the slippers. It assigns the resulting list of moves to the `List2` variable. 
The function then uses the `append` predicate to concatenate these two lists of moves into a single list, which is stored in the `List3` variable. 
Finally, it calls the `remove_duplicates` predicate to remove any duplicate moves from this list, and it assigns the resulting list to the `ListOfMoves` variable, which is returned as the output of the predicate.

For player p2, the function follows a similar process, but it uses the `findall` predicate to find all the black stones (jumpers and slippers) belonging to player p2, and it determines the possible moves that can be made with these stones.
The resulting list of moves is then concatenated and de-duplicated in the same way as for player p1.

### **Fim do Jogo**

O predicado `game_over(+GameState)` verifica se o estado atual do jogo é um estado de fim do jogo. Encontramo-nos num estado de fim do jogo se o peão neutro ficar preso. 

Se assim for, o predicado `find_out_winner/2` irá calcular o vencedor do jogo e o predicado `congratulate/1` irá anunciar o vencedor do jogo e a respetiva pontuação.

### **Game state evaluation**

The `value(+GameState, -Value)` predicate takes two arguments: the current game state (represented as a list) and a value (represented as a variable).
The function calls the `valid_moves` predicate to get the number of valid moves that can be made by the current player and the opponent player. It then calculates the difference between these two values and divides the result by 8.
This resulting value is then returned as the output of the `value` predicate.

### **Computer move**

The `choose_move(+GameState, +Level, -Move)` predicate takes three arguments, the current game state (represented as a list) that also contains the next player to make a move, the level of difficulty of that player and the Move that it will make (this is represented as a list just such as `[X1,Y1,X2,Y2]`, being `*1` the stone origin and `*2` the stone destination cell).
The level of difficulty can take one of the following values:

- **1** if it is an easy difficulty computer. Chooses randomly a move from the list of valid moves using the library `random` to choose an index of that list (with the predicates `random_index` and `nth0`);
- **2** if it is an hard difficulty computer. Uses the predicate `greedy_evaluation` that goes recursively through every move in the list of moves and retrieve the one that has the biggest value for the computer, by simmulating all moves possible.

`greedy_evaluation(+GameState, +ListOfMoves, -BestMove, -BestValue)` predicate takes four arguments: the current game state (represented as a list), a list of moves, the best move (represented as a variable), and the best value (represented as a variable). The function is defined twice, once for the case where the list of moves has a single element and once for the case where it has more than one element.

For the case where the list of moves has a single move, the function calls the `simulate_move` predicate to determine the value of making this move from the current game state. It then assigns this value to the `BestValue` variable and the move to the `BestMove` variable, which are returned as the output of the predicate.

For the case where the list of moves has more than one move, the function calls the `simulate_move` predicate to determine the value of making the first move in the list. It then calls itself recursively (using the `greedy_evaluation` predicate) to determine the best move and value from the remaining moves in the list.
Finally, it compares the value of the first move to the value of the best move from the remaining moves.
- If the value of the first move is greater, it assigns this move and value to the `BestMove` and `BestValue` variables.
- If the value of the first move is not greater, it assigns the best move and value from the remaining moves to these variables.

This process continues until all the moves in the list have been evaluated, and the final result is returned as the output of the predicate.

## **Conclusion**

The board game *Ski Jumps* was successfully implemented in the SicStus Prolog 4.7.1 language. The game can be played Player vs Player, Player vs Computer or Computer vs Computer (with the same or different levels).

One of the difficulties on the project was displaying an intuitive board in the SicStus terminal, which has a very limited set of characters and customization. This limits the game design, since it's hard to display black/white cells and black/red stones at the same time.

Also, we would've liked to have further time to develop the game and add the possibility for the user to choose the board dimensions.

Regardless, the development of *Ski Jumps* was crucial for the deepening of our knowledge on the Prolog language, and we may have ended up understanding the need for such a language in our paths as computer engineers.
