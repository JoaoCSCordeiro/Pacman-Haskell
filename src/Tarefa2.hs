{- |

= Introdução 

Na tarefa 2 o objetivo foi dado uma descrição do estado do jogo e uma jogada de um jogador, determinar se a jogada é valida e executar o efeito dessa jogada no estado do jogo, 
isto para todos os jogadores do tipo Pacman e Ghost. 

= Solução

Para esta tarefa foi implementado um tipo Data para definir os diferentes tipos de movimentação consoante a sua orientação
Foi também implementado um tipo Data para definir o estado do labirinto que consistia na estrutura do labirinto, um playerstate que continha a lista de todos os jogadores
e o nível em que o jogo se encontra, foi também definido um tipo Data para o jogador, que poderia ser um Pacman ou um fantasma, nesta tarefa o foco foi os jogadores tipo Pacman
que é definido por um tipo de dados, PacState que nos da informações sobre o seu tempo restante em modo Mega, o estado da sua boca (aberta ou fechada) e o seu modo
(Mega, Normal ou Dying). O playerstate da nos informações relativas ao ID do jogador, a sua posição, a velocidade, orientação, pontos e o numero de vidas
O processamento da jogada foi sujeito a regras, quando o jogador se movimenta para uma direção diferente da sua orientação devia-se manter na mesma posição alterando apenas
a sua orientação, caso contrário este deverá avançar uma posição da direção imposta pelo jogador.
Para processar as diferentes jogadas usamos uma função PlayerActions para cada orientação diferente que recebia um jogador, um labirinto e processava o que acontecia em todas
as diferentes possibilidades de estado do labirinto 
Foram necessárias funções extra para casos especificos como quando o Pacman bate contra um fantasma usando a função testeF que aquando o estado do Pacman e do Ghost
atualiza-os depois da colisão, foram necessária também duas funções, funcaoTunelR e funcaoTunelL, para fazer o Pacman atravessar o tunel quando este o atravessava.

= Conclusão 

Com isto somos capazes de executar jogadas com o Pacman fazendo-o reagir apropriadamente às diferentes situações de jogo. 

-}

module Tarefa2 where 
import Types  
import FileUtils

-- | dado um labirinto e um jogador dá-nos a peça que se encontra nas coordenadas do jogador
quePeca :: Maze -> PlayerState -> Piece                        
quePeca m (i,(0,0),a,d,z,l) = (head (head m))
quePeca ((x:xs):y) (i,(b,c),a,d,z,l)
 | b==0 = quePeca1 (x:xs) (i,(0,c),a,d,z,l)
 | b/=0 = quePeca y (i,(b-1,c),a,d,z,l)

-- | função auxiliar á função quePeca que procura a peça no corredor
quePeca1 :: Corridor -> PlayerState -> Piece                   
quePeca1 (x:xs) (i,(0,c),a,d,z,l)
 | c==0 = x
 | c/=0 = quePeca1 xs (i,(0,c-1),a,d,z,l)

-- | obtem o labirinto a partir de um state
getMazeFromState :: State -> Maze 
getMazeFromState (State m l n) = m 

-- | atualiza as coordenadas do jogador  quando este entra no tunel á direita           
funcaoTunelR :: Player -> Maze -> Player                                                                   
funcaoTunelR (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) maze 
  | (x,y)== (x,(length (head maze))-1) = (Pacman (PacState (id, (x,0), v, o ,p, l) t oc m))
  | otherwise = (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m))

-- | atualiza as coordenadas do jogador  quando este entra no tunel á esquerda 
funcaoTunelL :: Player -> Maze -> Player                                                         
funcaoTunelL (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) maze 
  | (x,y)==(x,0)  = (Pacman (PacState (id,(x,(length (head maze))-1) , v, o ,p, l) t oc m))
  | otherwise= (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m))

-- | atualiza o estado do fantasma com o pacman fica no estado Mega 
mataFantasmas :: Player -> Maze -> [Player] -> [Player]                            
mataFantasmas p lab [] = []
mataFantasmas pl@(Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) lab (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m'):t)        
 | quePeca lab (id, (x,y), v, o ,p, l) == Food Big = (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') Dead)):(mataFantasmas pl lab t) 
 | otherwise = (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') Alive)):t


-- | atualiza o estado do jogador caso ele colida com um fantasma 
testeF :: Player -> Player -> [Player] -> Player                                                                
testeF (Pacman (PacState (id2, (x2,y2), v2, o2 ,p2, l2) b2 oc2 m2)) (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) [] = Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)
testeF (Pacman (PacState (id2, (x2,y2), v2, o2 ,p2, l2) b2 oc2 m2)) (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m'):t)   
 | m==Normal && x == x1 && y == y1 && l==0 && m' == Alive = (Pacman (PacState (id2, (x2,y2), v2, o2 ,p2, 0) b2 oc2 Dying)) 
 | m==Normal && x == x1 && y == y1 && m' == Alive = (Pacman (PacState (id2, (x2,y2), v2, o2 ,p2, l-1) b2 oc2 m2)) 
 | m==Mega && x == x1 && y == y1 && m' == Dead = (Pacman (PacState (id2, (x,y), v, o ,p+10, l) b oc m)) 
 | otherwise = testeF (Pacman (PacState (id2, (x2,y2), v2, o2 ,p2, l2) b2 oc2 m2)) (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) t 

-- | atualiza o estado do pacman quando este realiza ações para cima
playerActionsU :: Player -> Maze -> Player                                                    
playerActionsU (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze 
                                  | coiso == Empty = (Pacman (PacState (id, (x-1,y), v, o ,p, l) b oc m))
                                  | coiso == Food Little = (Pacman (PacState (id, (x-1,y), v, o ,p+1, l) b oc m))
                                  | coiso == Food Big = (Pacman (PacState (id, (x-1,y), v, o ,p+5, l) 10 oc Mega))
                                  | coiso == Wall = (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m))
                                  | otherwise = (Pacman (PacState (id, (x-1,y), v, o ,p, l) b oc m))
                                  where coiso = quePeca  maze (id, (x-1,y), v, o ,p, l)

-- | atualiza o estado do pacman quando este realiza ações para baixo
playerActionsD :: Player -> Maze -> Player                                                  
playerActionsD (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze 
                                  | coiso == Empty = (Pacman (PacState (id, (x+1,y), v, o ,p, l) b oc m))
                                  | coiso == Food Little = (Pacman (PacState (id, (x+1,y), v, o ,p+1, l) b oc m))
                                  | coiso == Food Big = (Pacman (PacState (id, (x+1,y), v, o ,p+5, l) 10 oc Mega))
                                  | coiso == Wall = (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m))
                                  | otherwise = (Pacman (PacState (id, (x+1,y), v, o ,p, l) b oc m))
                                  where coiso = quePeca  maze (id, (x+1,y), v, o ,p, l)
                                  
-- | atualiza o estado do pacman quando este realiza ações para a esquerda
playerActionsL :: Player -> Maze -> Player                                                 
playerActionsL (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze 
                                  | y == 0 = funcaoTunelL (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze
                                  | coiso == Empty = (Pacman (PacState (id, (x,y-1), v, o ,p, l) b oc m))
                                  | coiso == Food Little = (Pacman (PacState (id, (x,y-1), v, o ,p+1, l) b oc m))
                                  | coiso == Food Big = (Pacman (PacState (id, (x,y-1), v, o ,p+5, l) 10 oc Mega))
                                  | coiso == Wall = (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m))
                                  | otherwise =  (Pacman (PacState (id, (x,y-1), v, o ,p, l) b oc m))
                                  where coiso = quePeca  maze (id, (x,y-1), v, o ,p, l)
                                  
                                  
-- | atualiza o estado do pacman quando este realiza ações para a direita
playerActionsR :: Player -> Maze -> Player                                                 
playerActionsR (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze 
                                  | y == ((length(head maze))-1) = funcaoTunelR (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze
                                  | coiso == Empty = (Pacman (PacState (id, (x,y+1), v, o ,p, l) b oc m))
                                  | coiso == Food Little = (Pacman (PacState (id, (x,y+1), v, o ,p+1, l) b oc m))
                                  | coiso == Food Big = (Pacman (PacState (id, (x,y+1), v, o ,p+5, l) 10 oc Mega))
                                  | coiso == Wall = (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m))
                                  | otherwise = (Pacman (PacState (id, (x,y+1), v, o ,p, l) b oc m))
                                  where coiso = quePeca  maze (id, (x,y+1), v, o ,p, l)
                                  

-- | junta a s funções playerAction todas 
action :: Player -> Maze -> Orientation -> Player                                      
action (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) maze ori 
 | ori == R && o == R = playerActionsR (Pacman (PacState (id, (x,y), v, o ,p, l) b (abreFecha oc) m)) maze 
 | ori == L && o == L = playerActionsL (Pacman (PacState (id, (x,y), v, o ,p, l) b (abreFecha oc) m)) maze 
 | ori == U && o == U = playerActionsU (Pacman (PacState (id, (x,y), v, o ,p, l) b (abreFecha oc) m)) maze 
 | ori == D && o == D = playerActionsD (Pacman (PacState (id, (x,y), v, o ,p, l) b (abreFecha oc) m)) maze 
 | otherwise = (Pacman (PacState (id, (x,y), v, ori ,p, l) b oc m))


-- | atualiza o labirinto deixando um espaço branco por onde o pacman passa
updateMaze :: Player -> [Player] -> Maze -> Orientation -> Maze                                                        
updateMaze (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) lpac lab R = placePlayersOnMap lpac (replaceElemInMaze (x,y) Empty lab)
updateMaze (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) lpac lab L = placePlayersOnMap lpac (replaceElemInMaze (x,y) Empty lab)
updateMaze (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) lpac lab U = placePlayersOnMap lpac (replaceElemInMaze (x,y) Empty lab)
updateMaze (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) lpac lab D = placePlayersOnMap lpac (replaceElemInMaze (x,y) Empty lab)
updateMaze _ lpac lab _ = lab 



-- | mete os fantasmas no meio da casa de fantatsmas qunado ele morre 
meioF :: Player -> [Player] -> Maze -> [Player]                               
meioF (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) [] lab = []
meioF (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m'):xs)  lab 
 | x==x1 && y==y1 && m==Mega && m'==Dead = (Ghost(GhoState (i, ((div (length lab) 2 ),(div (length (head lab)) 2)), v', o' ,p', l') Alive)) : meioF (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) xs lab
 | otherwise = (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m')) : meioF (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) xs lab

-- | distingue o jogador pelo ID
distingueP :: Int -> [Player] -> Player                                                                            
distingueP a (p:t) 
 | a==getPlayerID p = p
 | otherwise = distingueP a t 


-- | distingue se é fantasma
distingueF :: [Player] -> [Player]                                       
distingueF ((Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')):tail) = (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')): distingueF tail
distingueF ((Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)):tail) = distingueF tail 
distingueF [] = []


-- | junta os pacmans e os ghosts numa lista 
juntaPG :: Player -> [Player] -> [Player]                                             
juntaPG (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) lp = (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) : lp

-- | se o jogador da lista tiver o mesmo id que o id dado, esse jogador vai ser retirado da lista
tira :: [Player] -> Int -> [Player]
tira [] id = []
tira (x:xs) id 
 | getPlayerID x == id = xs 
 | otherwise = x: tira xs id  


-- | executa uma jogada dado um Maze state 
play :: Play -> State -> State
play (Move i d) (State m ps l) 
  | getPlayerID (distinguep ps) == i = (State ( updateMaze (distingueP i ps) lpac m d ) lpac l)
  | otherwise = let ghost = distingueP i ps 
                    lp = tira ps i 
                    ghost1 = actionF ghost m d 
                    lp1 = ghost1:lp 
                in State m lp1 l 
 where lpac = (juntaPG (testeF (distingueP i ps) (action  (distingueP i ps) m d) (distingueF ps)) (mataFantasmas (action  (distingueP i ps) m d) m (meioF (testeF (distingueP i ps) (action  (distingueP i ps) m d) (distingueF ps)) (distingueF ps) m )))
       

-- | atualiza o estado do Fantasma quando este realiza ações para baixo
playerActionsUF :: Player -> Maze -> Player                                                    
playerActionsUF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze                                
                                  | coiso == Wall = (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) 
                                  | otherwise = (Ghost(GhoState (id1, (x1-1,y1), v1, o1 ,p1, l1) m')) 
                                  where coiso = quePeca  maze (id1, (x1-1,y1), v1, o1 ,p1, l1)

-- | atualiza o estado do Fantasma quando este realiza ações para baixo
playerActionsDF :: Player -> Maze -> Player                                                  
playerActionsDF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze
                                  | coiso == Wall = (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m'))
                                  | otherwise = (Ghost(GhoState (id1, (x1+1,y1), v1, o1 ,p1, l1) m'))
                                  where coiso = quePeca  maze (id1, (x1+1,y1), v1, o1 ,p1, l1)
                                  
-- | atualiza o estado do Fantasma quando este realiza ações para a esquerda
playerActionsLF :: Player -> Maze -> Player                                                 
playerActionsLF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze 
                                  | coiso == Wall = (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m'))
                                  | otherwise =  (Ghost(GhoState (id1, (x1,y1-1), v1, o1 ,p1, l1) m'))
                                  where coiso = quePeca  maze (id1, (x1,y1-1), v1, o1 ,p1, l1)
                                  
                                  
-- | atualiza o estado do Fantasma quando este realiza ações para a direita
playerActionsRF :: Player -> Maze -> Player                                                 
playerActionsRF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze
                                  | coiso == Wall = (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m'))
                                  | otherwise = (Ghost(GhoState (id1, (x1,y1+1), v1, o1 ,p1, l1) m'))
                                  where coiso = quePeca  maze (id1, (x1,y1+1), v1, o1 ,p1, l1)

-- | junta todas a playerActions dos fantasmas 
actionF :: Player -> Maze -> Orientation -> Player                                      
actionF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze ori 
 | ori == R && o1 == R = playerActionsRF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze 
 | ori == L && o1 == L = playerActionsLF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze 
 | ori == U && o1 == U = playerActionsUF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze 
 | ori == D && o1 == D = playerActionsDF (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')) maze 
 | otherwise = (Ghost(GhoState (id1, (x1,y1), v1, ori ,p1, l1) m'))


abreFecha :: Mouth -> Mouth
abreFecha b
 | b == Closed = Open
 | otherwise = Closed









