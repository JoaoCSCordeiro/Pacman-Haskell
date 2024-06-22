{- | 
 
= Introdução 

O objetivo da tarefa 5 é automatizar o movimento dos fantasmas de modo a que estes executem a jogada ótima de maneira a dificultar o jogo ao Pacman.

 = Solução 

Para isto foi usada uma função isGhostAlive que detetava se existia um fantasma vivo, caso isto fosse verdade o objetivo dos fantasmas seria perseguir o Pacman caso este
estivesse em modo Normal que foi identificado usando a função isPacMega, para encontrar a jogada otima para o fantasma perseguir o Pacman foi usada a função orifav.
Caso contrário foi usada a função orifavmaior que encontra a jogada ótima para os fantasmas fugirem do pacman.
Na função final, ghostPlay, é detetado o estado do Pacman e aquando isto é escolhida entre a função scatterMode e chaseMode qual a melhor para o movimento do Ghost.

= Conclusão

Com isto conseguimos automatizar o movimento dos fantasmas de maneira a dificultar o jogo. 
-}

module Tarefa5 where 

import FileUtils
import Data.List 
import Tarefa2
import Types


-- | define a melhor orientação para aproximar o fantasma do pacman
caminhoF ::  Coords -> Coords -> Orientation
caminhoF (x,y) (x1,y1) 
 | x1 - x > 0 = D
 | x1 - x < 0 = U
 | y1 - y > 0 = R
 | y1 - y <0 = L
 | otherwise = Null


-- | vê se a peça é uma parede
parede :: Maze -> PlayerState -> Bool 
parede m (i,(x,y),a,d,z,l) = quePeca m (i,(x,y),a,d,z,l) == Wall 

-- | dada uma lista de playerstate dá uma lista de coordenadas
pc :: PlayerState -> Coords
pc (i,(x,y),a,d,z,l) = (x,y)

-- | esta função vai devolver a lista de todas as coordenadas possiveis para as qual o fantasmam se pode mover 
coordF :: PlayerState -> Maze -> [Coords]
coordF (id, (x,y), v, o ,p, l) m = filtrarParede m [(id, (x+1,y), v, o ,p, l),(id, (x-1,y), v, o ,p, l),(id, (x,y+1), v, o ,p, l),(id, (x,y-1), v, o ,p, l)]

-- | esta função vai devolver uma lista de coordenadas em que nenhuma delas corresponde a uma parede 
filtrarParede :: Maze-> [PlayerState]-> [Coords]
filtrarParede m [] = []
filtrarParede m ((id, (x,y), v, o ,p, l):xs)
 | parede m (id, (x,y), v, o ,p, l) = filtrarParede m xs
 | otherwise = (x,y) : filtrarParede m xs 

-- | compara as coordenadas todas para onde o fantasma pode ir com as coordenadas do pacman, e dá-me um tuplo em que o primeiro elemento corresponde á disância entre coordenadas e o segundo corresponde a coordenada do fantasma associada a essa distância
comparaCoords :: [Coords] -> Coords -> [(Float,Coords)]
comparaCoords [] (x1,y1) = [] 
comparaCoords ((x,y):t) (x1,y1) = (abs(sqrt(fromIntegral((x1-x)^2+(y1-y)^2))),(x,y)) : comparaCoords t (x1,y1)

-- | pega na lista de tuplos em que o primeiro corresponde á distâncias e segundo ás coordenadas e devolve as coordenadas associadas á menor distância 
menordist :: [(Float,Coords)] -> Coords 
menordist l = snd $ head $ sortOn fst l  

-- | devolve a coordenanda que está associada á menor distância entre o fantasma e ghost
finalmenor :: PlayerState -> Maze -> Coords -> Coords
finalmenor p lab (x,y) = menordist $ comparaCoords (coordF p lab) (x,y)

-- | dádo um jogador devolve o estado do jogador
getPlayerstate :: Player -> PlayerState
getPlayerstate (Pacman (PacState (id, (x,y), v, o ,p, l) b oc m)) = (id, (x,y), v, o ,p, l)
getPlayerstate (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m')) = (i, (x1,y1), v', o' ,p', l')

-- | dáda a coordenada associada á menor distância entre o fantas e o pacman devolve a orientação que o fantasma deve tomar para se aproximar do pacman
orifav :: Coords -> Player -> Maze -> Orientation 
orifav (x,y) (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m')) lab = caminhoF (x1,y1) bc 
  where ps =  (i, (x1,y1), v', o' ,p', l') 
        bc = finalmenor ps lab (x,y) 

{--
-- | ver se é pacman
distinguep :: [Player] -> Player                                         
distinguep ((Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)):tail) = (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m))
distinguep ((Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m')):tail) = distinguep tail 
--}

-- | dado um id e uma lista de players devolve o jogador com o id igual ao dado
idP :: Int -> [Player] -> Player                                                                                
idP a (p:t) 
 | a==getPlayerID p = p
 | otherwise = idP a t 

-- | usando as funções acima defenidas esta função dá-nos a melhor jogada pra o fantasma se aproximar do pacman do pacman     
chaseMode :: State -> Int -> Play
chaseMode (State lab xs l) id = (Move id o)
 where o = orifav cp pf lab 
       cp = getPlayerCoords $ distinguep xs 
       pf = idP id xs 


-- | define a melhor orientação para afastar o fantasma do pacman
caminhoP ::  Coords -> Coords -> Orientation
caminhoP (x,y) (x1,y1) 
 | x1 - x > 0 = U
 | x1 - x < 0 = D
 | y1 - y > 0 = L
 | y1 - y <0 =  R
 | otherwise = Null 


-- | pega na lista de tuplos em que o primeiro corresponde á distâncias e segundo ás coordenadas e devolve as coordenadas associadas á maior distância 
maiordist :: [(Float,Coords)] -> Coords
maiordist l = snd $ last $ sortOn fst l

-- | devolve a coordenanda que está associada á maior distância entre o fantasma e ghost
finalmaior :: PlayerState -> Maze -> Coords -> Coords
finalmaior p lab (x,y) = maiordist $ comparaCoords (coordF p lab) (x,y)

-- | dáda a coordenada associada á maior distância entre o fantas e o pacman devolve a orientação que o fantasma deve tomar para se afastar do pacman
orifavmaior :: Coords -> Player -> Maze -> Orientation 
orifavmaior (x,y) (Ghost(GhoState (i, (x1,y1), v', o' ,p', l') m')) lab = caminhoP (x1,y1) bc 
 where ps = (i, (x1,y1), v', o' ,p', l')
       bc = finalmaior ps lab (x,y) 

-- | usando as funções acima defenidas esta função dá-nos a melhor jogada pra o fantasma se afastar do pacman do pacman
scatterMode :: State -> Int -> Play
scatterMode (State lab xs l) id = (Move id o)
 where o = orifavmaior cp pf lab 
       cp = getPlayerCoords $ distinguep xs 
       pf = idP id xs 


-- | juntamos a função chaseMode e scatterMode 
ghostPlay :: State -> [Play]
ghostPlay (State lab [(Pacman (PacState (id, (x,y), v, o ,p, l) t oc m))] lv) = []
ghostPlay (State lab [] lv) = []
ghostPlay (State lab  ((Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)):tail) lv) = ghostPlay (State lab (tail++[(Pacman (PacState (id, (x,y), v, o ,p, l) t oc m))]) lv) 
ghostPlay s@(State lab ((Ghost(GhoState (i', (x1,y1), v', o' ,p', l') m)):xs) lv) 
 | m == Alive = (chaseMode s i') : ghostPlay (State lab xs lv)  
 | otherwise = (scatterMode s i') : ghostPlay (State lab xs lv) 



