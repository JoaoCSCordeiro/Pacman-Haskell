{- | 

= Introdução 

O objetivo desta tarefa é automatizar por completo o jogador Pacman.

= Solução

Para isto foi usada uma estratégia similar à usada na tarefa 5 em que é usada a função ifpacMega que deteta o estado do Pacman,
se este for Mega ele vai procurar o movimento ótimo para perseguir os fantasmas usando a função aproximaPac,
caso contrário é usada a função fogepac para que o Pacman fuga dos fantasmas de modo e evitar a sua morte.

= Conclusão 

Mesmo achando que tinhamos a ideia certa não foi possivel implementar um robô funcional que jogassse Pacman automaticamente.
-}

module Tarefa6 where

import Tarefa5
import Types


-- | ver se o pacman está em estado mega 
ifpacMega :: Player -> Bool
ifpacMega (Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)) 
 | m == Mega = True
 | otherwise = False

-- | dada uma lista de players vai devolver uma lista só com fantasmas
distingueF :: [Player] -> [Player]                                       
distingueF ((Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')):tail) = (Ghost(GhoState (id1, (x1,y1), v1, o1 ,p1, l1) m')): distingueF tail
distingueF ((Pacman (PacState (id, (x,y), v, o ,p, l) t oc m)):tail) = distingueF tail 
distingueF [] = []

-- | dada uma lista de jogadores vai nos ser devolvida uma lista com as coordenadas de cada jogador
coordsplay :: [Player] -> [Coords]
coordsplay (x:xs) = getPlayerCoords x : coordsplay xs 

 
-- | nesta função é dado um pacman em modo normal, e devolve a melhor jogada possível para que pacman fique o mais longe possível do fantasma
fogepac :: State -> Int -> Play
fogepac (State m ps lv) id
 | ifpacMega pc == False = (Move id o)
 where o = orifavmaior cfu pc m
       cf = coordsplay $ distingueF ps 
       pc =  distinguep ps 
       cfu = menordist $ comparaCoords cf pac  
       pac = getPlayerCoords pc 


-- | nesta função é dado um pacman em modo Mega , e devolve a melhor jogada possível para que pacman fique o mais perto possível do fantasma
aproximapac :: State -> Int -> Play
aproximapac (State m ps lv) id
 | ifpacMega pc == True = (Move id o)
 where o = orifav cfu pc m
       cf = coordsplay $ distingueF ps 
       pc = distinguep ps 
       cfu = maiordist $ comparaCoords cf pac 
       pac = getPlayerCoords pc 


-- | nesta função juntam-se a função fogepac e aproximapac, para consoante o estado de jogo seja dada a jogada nais favorável
bot :: Int -> State -> Maybe Play
bot x (State [] ps lv) = Nothing
bot x (State m [] lv) = Nothing 
bot x s@(State m ps lv)
 | ifpacMega (idP x ps) = Just (aproximapac s x) 
 | otherwise = Just (fogepac s x)




