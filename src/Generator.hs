module Generator where

import System.Random

import Types

            


-- | Given a seed returns a list of n integer randomly generated
--
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Given a seed returns an integer randomly generated
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- Converts a list into a list of list of size n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


--converter inteiro em Food
convertePiece :: Int -> Piece 
convertePiece p
  | p == 3 = Food Big
  | p>=0 && p<70 = Food Little
  | p>=70 && p<=99 = Wall

-- | Converts a Corridor to a string
--juntar Strings: ++
printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (x:xs) = show x ++ printCorridor xs 


tamanhoLista :: [Int] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista xs 

-- | Converts a Maze to a string
--
--printMaze :: Maze -> String
--printMaze [] = []
--printMaze (x:xs) = printCorridor x ++ printMaze xs 


-- | Converts a list of integers into a Corridor
--
converteCorridor :: [Int] -> Corridor
converteCorridor [] = []
converteCorridor (x:xs) = convertePiece x : converteCorridor xs


-- | Converts a list of lists of integers into a Maze
--
converteMaze :: [[Int]] -> Maze
converteMaze [] = []
converteMaze (x:xs) = converteCorridor x : converteMaze xs


geraMaze :: Int -> Int -> Int -> Maze
geraMaze x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in converteMaze $ subLista x random_nrs

imprimeMaze :: Maze -> IO ()
imprimeMaze l = do putStrLn ( printMaze ( l ))

