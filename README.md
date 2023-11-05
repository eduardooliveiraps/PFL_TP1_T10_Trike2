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

### **Lista de jogadas válidas**

O predicado `valid_moves(+GameState, +Player, -ListOfMoves)` é responsável por guardar em ListOfMoves as coordenadas das posições que correspondem a movimentos válidos para o estado atual do jogo. 

Para isso, utiliza o predicado `findall` que utilizando o predicado `validate_move` guarda em ListOfMoves as coordenadas que correspondem a jogadas possíveis.

### **Fim do Jogo**

O predicado `game_over(+GameState)` verifica se o estado atual do jogo é um estado de fim do jogo. Encontramo-nos num estado de fim do jogo se o peão neutro ficar preso. 

Se assim for, o predicado `find_out_winner/2` irá calcular o vencedor do jogo e o predicado `congratulate/1` irá anunciar o vencedor do jogo e a respetiva pontuação.

### **Avaliação do estado do jogo**

O predicado `value(+GameState, +Player, -Value)` é responsável por avaliar o estado atual do jogo e guarda na variável `Value` o valor do tabuleiro atual. Este valor é calculado com base no jogador atual, na posição do peão neutro e nas peças que o rodeiam.

O predicado value/3 analisa todas as peças que rodeiam o peão neutro. Para uma peça do tipo do jogador atual, a variável `Value` é incrementada em 1 valor. No caso do tipo da peça ser do adversário, a variável `Value` é decrementada em 1 valor.

### **Jogadas do Computador**

Para o computador decidir qual o movimento a realizar, foram realizados dois métodos:random e greedy.

O método random, como o nome indica, apenas escolhe de forma aleatória um movimento da lista de movimentos válidos.

O método greedy utiliza o predicado de avaliação do `tabuleiro value/3`, bem como o predicado `check_possibility_of_winning/6` que verifica se existe alguma posição que leve o computador à vitória. Se existir alguma posição que leve o computador à vitória, então o computador realiza essa movimentação. Senão, o computador realiza o seu movimento com base no predicado `value/3`, escolhendo a posição com o maior `Value`. Se existirem várias posições que levem ao mesmo `Value`, é escolhida uma dessas posições de forma aleatória.

## **Conclusion**

O jogo de tabuleiro Trike foi implementado pelo nosso grupo com sucesso em Prolog. 

As maiores dificuldades sentidas foram na utilização da linguagem Prolog num jogo relativamente complexo como o nosso, com um tabuleiro triangular e com uma série de regras exigentes, bem como na implementação do código do computador que realiza as suas jogadas de forma greedy/inteligente.

Apesar das dificuldades sentidas, conseguimos cumprir com todos os tópicos pedidos no guião do projeto.

Concluindo, consideramos que este projeto foi importante para desenvolvermos as nossas capacidades de programação em prolog, bem como o nosso raciocínio lógico.

## **Bibliografia**

[website oficial do jogo](https://boardgamegeek.com/boardgame/307379/trike)
