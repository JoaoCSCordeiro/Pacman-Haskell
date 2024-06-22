{- |
= Introdução 

Na tarefa 1 o objetivo foi a criação de uma função que dada o numero de corredores horizontais e o seu comprimento, bem como um numero inteiro positivo para usar como semente
num gerador pseudoaleatório, que gera um labirinto, formado por diferentes tipos de peças que determinam o caminho por onde o jogador pode andar e maneiras diferentes de ganhar 
pontos, este labirinto também deveria estar dentro de certos parâmetros estabelecidos previamente.
Estes parâmetros eram a inclusão de um túnel em cada extremidade central do labirinto que deveria ser formada por 1 corredor caso a altura do labirinto fosse impar ou 2
corredores caso a altura do labirinto fosse par.
Um labirinto valido também deveria incluir uma casa de fantasmas com altura de 3, um comprimento de 9 caso o comprimento do labirinto fosse impar ou 8 caso o comprimento do 
labirinto fosse par, esta casa também deveria ser rodeada por espaços vazios.

= Objetivos

Para alcançar isto foi usada uma função que trocava dois corredores dados os mesmos e um inteiro que representava a posição onde deveria começar a troca, ReplaceInCorridor,
o que nos permitiu substituir os corredores do meio com a casa dos fantasmas e o túnel, localizando os mesmos com a função, get5MiddleCorridor.
Para gerar um labirinto aleatório usámos as funções tunel, que substitui no sitio adequado as paredes da extremidade central do labirinto por um tunel, funçãof, que coloca no 
centro do labirinto uma casa dos fantasmas, e geralablimites que gera aleatoriamente um labirinto rodeado de paredes onde serão aplicadas as outras 2 funções.

= Conclusão

Com isto somos capazes de gerar um labirinto semi-aleatório que obedece aos parâmetros estabelecidos no enunciado.
-}

module Tarefa1 where

import Generator
import Types

-- | constroi um corredor só com paredes horizontais 
muralha :: Int -> Corridor                                      
muralha 0 = []
muralha x = Wall : muralha (x-1)


-- | constroi um corredor so com paredes verticais
muralhaV :: Maze -> Maze                                  
muralhaV [] = []
muralhaV (x:xs) = [[Wall] ++ x ++ [Wall]] ++ muralhaV xs


-- | gera um labirinto delimitado por paredes verticalmente e horizontalmente 
geraLabLimites :: Int -> Int -> Int -> Maze                         
geraLabLimites 0 0 0 = []
geraLabLimites x y z = muralhaV  (geraLab (y-2) (x-2) z)


-- | adiciona um dado corredor a um dado labirinto 
addCorridor :: Maze -> Corridor -> Maze                               
addCorridor [] c = [c]
addCorridor (x:xs) c = x : addCorridor xs c

-- | gera um labirinto  onde o primeiro e ultimo corredor sao paredes
geraLab :: Int -> Int -> Int -> Maze                                           
geraLab a l s = addCorridor ((muralha l):(geraMaze l (a-2) s)) (muralha l)


-- | gera uma casa de fantasmas baseada na altura do labirinto  
casaFantasma :: Int -> Maze                                                       
casaFantasma x 
 | even x = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
             [Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty],
             [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
             [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]
 

 | otherwise = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty],
                [Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],
                [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
                [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty]]



-- | dado um labirinto diz a sua largura
nrColunas :: Maze -> Int                                                       
nrColunas (x:xs) = length (x) 


-- | dada uma lista e um numero, retorna o elemento que está na posicão do numero dado
exclamacao :: [a] -> Int -> a                                                    
exclamacao (x:xs) 0 = x 
exclamacao (x:xs) n = exclamacao xs (n-1) 

-- | dado um labirinto dá as suas 5 linhas do meio 
get5MiddleCorridor :: Maze -> [Corridor]                                            
get5MiddleCorridor (x:xs) = let meio = (div (length (x:xs)) 2) -1
                              in [exclamacao (x:xs) meio,exclamacao (x:xs) (meio+1),exclamacao (x:xs) (meio-1),exclamacao (x:xs) (meio+2),exclamacao (x:xs) (meio-2)]
                            

-- | dado dois Corredores vais substituir o primeiro no segundo começando na posição dada 
replaceInCorridor :: Int -> Corridor -> Corridor -> Corridor                             
replaceInCorridor x [] [a] = [a] 
replaceInCorridor x [a] [] = [a] 
replaceInCorridor x l (z:zs) 
                        | x == 0 = l ++ (drop (length l) (z:zs))
                        | x /= 0 = z : replaceInCorridor (x-1) l zs 



-- | dado um labirinto diz a sua altura
nrLinhas :: Maze -> Int                                          
nrLinhas (x:xs) = length (x:xs)



-- | dado um labirinto troca as suas 5 linhas do meio pela casa dos fantasmas
funcaof :: Maze -> Maze                                                     
funcaof [] = []
funcaof l      
              | even (length l) = take ((div (length l) 2)-2) l ++ 
                           [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 0 ) (exclamacao (get5MiddleCorridor l) 0 )] ++
                           [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 1 ) (exclamacao (get5MiddleCorridor l) 1 )] ++
                           [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 2 ) (exclamacao (get5MiddleCorridor l) 2 )] ++
                           [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 3 ) (exclamacao (get5MiddleCorridor l) 3 )] ++
                           [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 4 ) (exclamacao (get5MiddleCorridor l) 4 )] ++
                           (drop ((div (length l) 2)+1) l)

              | otherwise = take ((div (length l) 2)-1) l ++ 
                              [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 0 ) (exclamacao (get5MiddleCorridor l) 0 )] ++
                              [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 1 ) (exclamacao (get5MiddleCorridor l) 1 )] ++
                              [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 2 ) (exclamacao (get5MiddleCorridor l) 2 )] ++
                              [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 3 ) (exclamacao (get5MiddleCorridor l) 3 )] ++
                              [replaceInCorridor ((div (length (head l)) 2)-5)  (exclamacao (casaFantasma (length (head l) )) 4 ) (exclamacao (get5MiddleCorridor l) 4 )] ++
                               (drop ((div (length l) 2)+2) l)

-- | dado um labirinto troca as extremidades de uma (caso a altura seja impar) ou duas linhas (caso a altura seja par) do centro por chão              
tunel :: Maze -> Maze                                  
tunel [] = []
tunel l
 | even (length l) = take ((div (length l) 2)-1) l ++ 
                     [(Empty:(init (tail (exclamacao (get5MiddleCorridor l ) 0 ))) ++ [Empty])] ++ 
                     [(Empty:(init (tail (exclamacao (get5MiddleCorridor l ) 1 ))) ++ [Empty])] ++
                     drop ((div (length l) 2) + 1) l
 | otherwise = take ((div (length l) 2)) l ++ 
                     [(Empty:(init (tail (exclamacao (get5MiddleCorridor l ) 1 ))) ++ [Empty])] ++ 
                     drop ((div (length l) 2) + 1) l


-- | gera um labirinto válido incluindo a casa dos fantasmas no seu centro e o tunel nas extremidades do mesmo
generateMaze :: 
                Int ->                                                                                   
                  Int ->                                                   
                    Int -> Maze                                                                      
generateMaze a b c = tunel $ funcaof $ geraLabLimites a b c 
