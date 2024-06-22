{- |

= Introdução 

O objetivo desta tarefa é converter um labirinto valido em instruções de forma a ser mais compacto para leitura. Para ser valida a instrução não pode conter um numero maior
ou menor de peças que a largura do labirinto.

= Solução

As instruções foram definidas como um tuplo cujo primeiro valor é um inteiro e um segundo uma peça, estas serão usadas para escrever o labirinto “por extenso”
Para a máxima compactação do labirinto usamos funções que verificam se 2 corredores são iguais, , corredoresIguais, com o objetivo de substituir os corredores iguais apenas
por Repeat seguido da posição do corredor igual com a função addRepeat.

= Conclusão

Com isto somos capazes de compactar um labirinto com o tipo Instructions que obedece aos parâmetros estabelecidos no enunciado.
-}

module Tarefa3 where
import Types
import Tarefa1

-- | verifica se 2 corredores são iguais
corredoresIguais :: Corridor -> Corridor -> Bool 
corredoresIguais [] [] = True
corredoresIguais _ [] = False 
corredoresIguais [] _ = False 
corredoresIguais (x:xs) (y:ys) 
                          | x==y = corredoresIguais xs ys
                          | x /=y = False


-- | verifica se num labirinto ha corredores iguais a um dado corredor e guarda a sua posição
posCorredorMaze :: Maze -> Int -> Corridor -> Int 
posCorredorMaze [] a corr = a 
posCorredorMaze (x:xs) q y 
                    | corredoresIguais x y = q
                    | otherwise = posCorredorMaze xs (q+1) y


-- | compara dois labirintos e guarda a posicao dos corredores que são iguais 
comparaMaze :: Maze -> Maze -> [Int] 
comparaMaze _ [] = []
comparaMaze [] _ = [] 
comparaMaze (x:xs) l = (posCorredorMaze l 0 x) : (comparaMaze xs l) 

-- | dado um labirinto e dois inteiros, dá o corredor que se encontra na posião do primeiro numero inteiro 
encontraCorredor :: Int -> Int -> Maze -> Corridor 
encontraCorredor a b (x:xs) 
                  | a == b = x
                  | a /= b =  encontraCorredor (a+1) b xs


-- | dado um corredor e um inteiro, passa o corredor para uma lista de pares de inteiros e peças
corrParaInst :: Corridor -> Int -> [(Int,Piece)]
corrParaInst [x] y = [(y,x)] 
corrParaInst (x:t:xs) y 
                  | x == t = (corrParaInst (t:xs) (y+1)) 
                  | x /= t = (y,x) : (corrParaInst (t:xs) 1)


-- | dado um labirinto e um inteiro, passa o labirinto para o tipo Instruction
labParaInt :: Maze -> Int -> Instruction
labParaInt [] y = Instruct []
labParaInt x y = Instruct (corrParaInst (encontraCorredor 0 y x) 1)

-- | dado um labirinto um inteiro e uma lista de inteiros, substitui em corredores repetidos o tipo instructions por Repeat da posição do corredor que era repetido
addRepeat :: Maze -> Int -> [Int] -> Instructions 
addRepeat a b [] = [] 
addRepeat a b (x:xs) 
               | b == x = (labParaInt a x) : (addRepeat a (b+1) xs)
               | b /= x = (Repeat x) : (addRepeat a (b+1) xs)



-- | Dado um maze, transforma-o em tipo Instructions 
compactMaze :: Maze -> Instructions 
compactMaze [] = [] 
compactMaze x = addRepeat x 0 (comparaMaze x x)














