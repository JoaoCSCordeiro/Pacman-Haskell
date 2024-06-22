{- |

= Introdução 

O objetivo desta tarefa é calcular o efeito da passagem de um instante de tempo no estado de jogo.

= Solução

Para esta tarefa foi importada a biblioteca nCurses que foi usada para a movimentação do jogador atráves do pressionamento das setas no teclado.
Para simular a passagem do tempo foi implementado um tipo Data Manager que consiste no State do jogo, o identificador unico do jogador, o step que é um inteiro que
representa quantas iterações do jogo já passou, o before que representa o instante de tempo de tempo em que foi efetuada a última jogada, o delta que representa o tempo
decorrido em milissegundos desde a última jogada e o delay que representa o intervalo de tempo entre jogadas que foi estabelecido em 250 ms.
Para movimentar os jogadores foi usada a função jogaTodos que aquando o estado do jogador vai atualizar o labirinto de acordo com o estado dos fantasmas que podem perseguir
o Pacman ou fugir dele se estiver em estado Mega. Esta função joga todos foi depois utilizada na função passtime.

= Conclusão

Com isto conseguimos controlar o jogador com as setas do teclado e visualizar as jogadas no ecrâ consoante a passagem do tempo.
-}



module Tarefa4 where 
import Tarefa5
import Tarefa2
import Types

defaultDelayTime = 250 -- 250 ms

-- | se o jogador estiver em modo mega vamos tirar um ao tempo mega restante do jogador
iftimeMega :: Player -> Player 
iftimeMega (Pacman (PacState (id, or, v, o ,p, l) tm oc Mega)) = (Pacman (PacState (id, or, v, o ,p, l) (tm-0.25) oc Mega))

-- | vê se o jogador está em modo mega
isPacmanMega :: Player -> Bool
isPacmanMega (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m')) = False
isPacmanMega (Pacman (PacState (id, or, v, o ,p, l) tm oc mega))  
 | mega == Mega = True
 | otherwise = False 

-- | vê se o jogador é um fantasma
isGhost :: Player -> Bool
isGhost (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) = True
isGhost _ = False

-- | vẽ se o fantasma está vivo
isGhostAlive :: Player -> Bool
isGhostAlive (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m'))
 | m' == Alive = True
 | otherwise = False

isPacAlive :: Player -> Bool 
isPacAlive (Pacman (PacState (id, or, v, o ,p, l) tm oc mega))
 | l<0 = False 
 | otherwise = True  


-- | atualiza a lista de jogadores no state usando as fuções definidas anteriormente 
jogaTodos :: [Player] -> State -> Int -> State 
jogaTodos [] state i = state 
jogaTodos (x:xs) state i 
  | isGhost x && isGhostAlive x = let s3 = play (chaseMode state (getPlayerID x)) state
                               in jogaTodos xs s3 i
  | isGhost x && even i = let s3 = play (scatterMode state (getPlayerID x)) state
                               in jogaTodos xs s3 i
  | isGhost x = jogaTodos xs state i 
  | isPacmanMega x = let novo = iftimeMega x
                         s1 = play (Move (getPlayerID x) (getPlayerOrientation x)) state
                      in jogaTodos xs s1 i
  | isPacmanMega x == False = let s2 = play (Move (getPlayerID x) (getPlayerOrientation x)) state 
                                in jogaTodos xs s2 i  
 

passTime :: Int  -> State -> State
passTime x (State m ps lvl) = (jogaTodos ps (State m ps lvl)x)


-- | dá o jogador como um certo ID
playerID :: Int -> State -> Player
playerID i (State m (x:xs) l ) 
 | i == getPlayerID x = x
 | otherwise = playerID i (State m xs l)

-- | rodar o jogador para a orientação dada
rotatePlayer :: Player -> Orientation -> Player
rotatePlayer (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) or
 | or == R = (Pacman (PacState (id, (x,y), v, R ,p, l) t oc m))
 | or == L = (Pacman (PacState (id, (x,y), v, L ,p, l) t oc m))
 | or == U = (Pacman (PacState (id, (x,y), v, U ,p, l) t oc m))
 | or == D = (Pacman (PacState (id, (x,y), v, D ,p, l) t oc m))
 

-- | substituir um jogador atualizado na lista de jogadores do State
replacePlayerinState :: Player -> State -> State
replacePlayerinState p (State m (xs) l) = (State m (replacePlayerinlist p xs) l)

-- | substituir um jogador atualizado numa lista de jogadores
replacePlayerinlist :: Player -> [Player] -> [Player]
replacePlayerinlist x [] = []
replacePlayerinlist x (y:ys) = if getPlayerID x == getPlayerID y then (x:ys) else y : replacePlayerinlist x ys

