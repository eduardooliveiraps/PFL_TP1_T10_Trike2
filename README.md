# Trike

## Group 2
- Bruno Miguel de Siuéia Duvane - 202109244
- Eduardo Martins Oliveira - 202108690

## Descrição do Jogo
  Trike é um jogo de estratégia abstrata combinatória para dois jogadores. O jogo gira em torno de criar armadilhas, desmontar armadilhas do adversário e manobrar uma peça compartilhada (peão neutro) para a sua própria armadilha com o objetivo de vencer.
  
  Regras: 
  - O tabuleiro do jogo tem a forma de um triângulo equilátero, e é constituído por posições hexagonais.
  - Um peão neutro e peças pretas e brancas são usadas para jogar.
  - No início do jogo, o primeiro jogador escolhe uma cor e coloca uma peça em qualquer posição do tabuleiro, com o peão neutro em cima dela. Somente neste momento, o segundo jogador tem uma chance única de trocar de lado (mudar de cor) em vez de fazer um movimento normal.
  - O peão neutro pode mover qualquer número de posições vazias, em qualquer direção em linha reta, mas não pode mover-se ou saltar sobre as posições ocupadas.
  - Assim que um jogador coloca uma peça da sua cor numa posição permitida, o peão fica sobre essa mesma peça.
  - Quando o peão fica preso, o jogo termina.
  - No final do jogo, cada jogador ganha um ponto para cada peça da sua cor adjacente ou abaixo do peão. A pessoa com a maior pontuação vence. Não existem empates.
    
Nota: Esta informação encontra-se detalhada no [website oficial do jogo](https://boardgamegeek.com/boardgame/307379/trike)

## Lógica do Jogo
### Representação Interna do Estado do Jogo
  Em qualquer momento do jogo, o estado do jogo (GameState) é representado por uma lista de dois elementos: [Player,Board], onde Player é o jogador atual que realizará a próxima jogada e Board é o estado atual do tabuleiro.
  O Player é representado por um átomo que pode ser "p1" para o player 1 ou "p2" para o player 2, dependendo do jogador atual. Cada célula/posição do tabuleiro pode ser representada pelo átomo "n" para o peão neutro, "b" para uma peça branca, "p" para uma peça preta ou "blank" para uma posição vazia. 

