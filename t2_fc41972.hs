-------------------------------------------------------------------------------------------------------------------------------
--
-- 2nd Trabalho Individual de Principios de Programacao 2020/21
-- @author  -> Marcio Andre Goncalves Pereira Pires Moreira
-- Aluno nr -> 41972
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- A.1 Diferencas consecutivas

paresConsecutivos :: [Int] -> [(Int,Int)]
paresConsecutivos[] = []
paresConsecutivos[_] = []
paresConsecutivos (x:xs) = (x, head xs): paresConsecutivos xs 

-- COMENTÁRIOS

-- junta numa tupla e o head da lista e volta a chamar

--------------------------------------------------------------
--
-- Estudos
--
-- paresConsecutivos :: [a] -> [(a, a)]
-- paresConsecutivos xs = zip xs (tail xs)

-------------------------------------------------------------------------------------------------------------------------------
--
-- A.2 Diferencas consecutivas

diferencasConsecutivas :: Num a => [a] -> [a]
diferencasConsecutivas [] = []
diferencasConsecutivas (x:xs) = diferencasAux xs x

diferencasAux :: Num a => [a] -> a -> [a]
diferencasAux [] a = []
diferencasAux (x:xs) a = (x-a) : diferencasAux xs x

-- COMENTÁRIOS

-- funcao aux = diferenca head da lista e um (a)
-- funcao princ. usa funcao aux e usa a head com chamada recursiva 

--------------------------------------------------------------
--
-- Estudos
--
-- diferencasConsecutivas :: Num a => [(a, a)] -> [a]
-- diferencasConsecutivas xs = [b-a | (a,b) <- xs]
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- B.1 Jogadores de futebol

claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
claramentePior _ [] = False
claramentePior t (x:xs)
    | getSnd t < getSnd x && getThird t < getThird x  = True
    | otherwise = claramentePior t xs 

getSnd :: (a, b, c) -> b
getSnd (_,b,_) = (b)

getThird :: (a, b, c) -> c
getThird (_,_,c) = (c)

-- COMENTÁRIOS

-- getSnd devolve o snd valor
-- getThird devolve o third valor
-- comparar com guardas o segundo e terceiro valor 
-- se nah se verificar True faz chamada recursiva outra x 

--------------------------------------------------------------
--
-- Estudos
--
-- getSndThd :: (a,b,c) -> (b,c)
-- getSndThd (_,b,c) = (b,c)
-- getData :: [(a, b, c)] -> [(b, c)]
-- getData xs = [ getSndThd(x)| x<- xs ]
-- claramentePior2 x ys = [ ys | y <- [(getData (ys-1)), (getData (ys-2))..], (fst (getSndThd x) >= fst y && snd (getSndThd x) >= snd y)] 
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- B.2

filtroJogadores :: [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores [] = []
filtroJogadores xs = [ x | x<-xs, a <-[claramentePior x xs], a == False ] 

-- COMENTÁRIOS

-- usa a funcao claramentePior ja por si recursiva.
-- cria uma guarda com a == False
-- condicoes verificadas em cada (x:xs) entao adiciana a nova lista.

--------------------------------------------------------------
--
-- Estudos
--
-- filtroJogadores (x:xs)
--   | claramentePior x xs = [("Ronaldo", 10, 3)] 
--   | otherwise = filtroJogadores xs
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- C. Preencher o buraco

sudoku :: [[Int]]
sudoku = [ [3, 6, 4, 8, 7, 1, 2, 9, 5]
    , [7, 5, 2, 9, 3, 6, 1, 8, 4]
    , [8, 1, 9, 2, 5, 4, 7, 3, 6]
    , [5, 9, 6, 7, 1, 3, 4, 2, 8]
    , [4, 3, 1, 5, 8, 2, 6, 7, 9]
    , [2, 7, 8, 4, 6, 9, 3, 5, 1]
    , [6, 4, 5, 3, 2, 8, 9, 1, 7]
    , [9, 8, 3, 1, 4, 7, 5, 6, 2]
    , [1, 2, 7, 6, 9, 5, 8, 4, 0] ]

sudoku2 :: [[Int]]
sudoku2 = [ [3, 6, 4, 8, 7, 1, 2, 9, 5] 
    , [7, 5, 2, 9, 3, 6, 1, 8, 4]
    , [8, 1, 9, 2, 5, 4, 7, 3, 6]
    , [5, 9, 6, 7, 1, 3, 4, 2, 8]
    , [4, 3, 1, 5, 8, 0, 6, 7, 9]
    , [2, 7, 8, 4, 6, 9, 3, 5, 1]
    , [6, 4, 5, 3, 2, 8, 9, 1, 7]
    , [9, 8, 3, 1, 4, 7, 5, 6, 2]
    , [1, 2, 7, 6, 9, 5, 8, 4, 3]
    ]

preencherVazio :: [[Int]] -> Int
preencherVazio xs = 405 - getSumLinha xs

getSumLinha :: (Num p, Foldable t) => [t p] -> p
getSumLinha [] = 0
getSumLinha (x:xs) = sum x + getSumLinha xs

-- COMENTÁRIOS

-- podia ter usado o factorial ou a soma ou todas em simultaneo
-- funcao aux getSumLinha faz o sum de todas as linhas
-- preencherVazio verifica a diferenca entre o valor total suposto das 
-- linhas todas de um Sudoku valido e a soma obtida 

--------------------------------------------------------------
--
-- soma linha/coluna/quadrado = 45
-- factorial 9 = 362880
-- soma todos os valores = 405 
--
-------------------------------------------------------------------------------------------------------------------------------
