
paresConsecutivos :: [a] -> [(a, a)]
paresConsecutivos [] = []
paresConsecutivos [x] = init [(x,x)]
paresConsecutivos (x:y:xs) =(x,y):paresConsecutivos (y:xs)

-------------------------------------------------------------------------------------------------------------------------------

diferencasConsecutivas :: [Int] -> [Int]
diferencasConsecutivas [] = []
diferencasConsecutivas [x] = init [x]
diferencasConsecutivas (x:y:xs) = y-x : diferencasConsecutivas (y:xs)

{- Funções auxiliares -}        
third :: (a,b,c) -> c
third (_,_,c) = c

second :: (a,b,c) -> b
second (_,b,_) = b

-------------------------------------------------------------------------------------------------------------------------------

claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
claramentePior _ [] = False
claramentePior p (t:ts) 
                        | second p< second t && third p < third t = True
                        |otherwise =claramentePior p ts

-------------------------------------------------------------------------------------------------------------------------------

filtroJogadores :: [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores [] = []
filtroJogadores xs = jogadores xs (length xs) 0

pior :: (String,Int,Int) -> (String,Int,Int) -> Bool
pior x y = if second x < second y && third x < third y then True else False

--verifica se existe algum jogador da lista claramente melhor que um jogador
varJogador :: (String,Int,Int) -> [(String,Int,Int)] -> Bool
varJogador _ [] = True
varJogador y (x:xs)
             |  not (pior y x) =  varJogador y xs
             | otherwise = False

--Vai retirar um jogador de uma lista de jogadores
takeJogador :: (String,Int,Int) -> [(String,Int,Int)] -> [(String,Int,Int)]
takeJogador _ [] = []
takeJogador y (x:xs) 
              | x /= y = x:takeJogador y xs
              |otherwise =takeJogador y xs      

--ver se ha algum jogador melhor dentro nessa lista
jogadores :: [(String,Int,Int)]-> Int -> Int ->[(String,Int,Int)]
jogadores xs n c 
        | n == c = []
        |varJogador (xs !! c)  (takeJogador ( xs !! c ) xs)= [xs !! c ] ++ jogadores xs n (c+1)
        |otherwise =jogadores xs n (c+1)

-------------------------------------------------------------------------------------------------------------------------------

preencherVazio :: [[Int]] -> Int
preencherVazio (x:xs)
              | contem x 0 = (takeValue (returnList x))
              |otherwise = preencherVazio xs


--Verifica se o valor esta ou nao na lista, se nao tiver ele é devolvido
naoContem' :: Int -> [Int] -> Int
naoContem' _ [] = 0
naoContem' x xs 
          | not (x `elem` xs ) = x
          |otherwise = 0

--Verifica se um elemnto esta na lista
contem :: [Int] -> Int -> Bool
contem [] _ = False
contem xs i 
        | i `elem` xs = True
        |otherwise = False

--Devolve a lista com o valor que falta
returnList :: [Int] -> [Int]
returnList xs = [ x | y<-[1..9] , let x = naoContem' y xs ]

--retira os valores que nao sejam zero da lista
takeValue :: [Int] -> Int
takeValue [] = 0
takeValue (x:xs)
            | x /= 0 = x
            |otherwise =takeValue xs





