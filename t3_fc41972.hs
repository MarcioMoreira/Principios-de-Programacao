------------------------------------------------------------------------------------------------------------------------------
--
-- 3rd Trabalho Individual de Principios de Programacao 2020/21
-- @author  -> Marcio Andre Goncalves Pereira Pires Moreira
-- Aluno nr -> 41972
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- A. LISTAS DE ALGUIDARES
--
-------------------------------------------------------------------------------------------------------------------------------
-- A.1

listaDeAlguidaresVazia :: [a]
listaDeAlguidaresVazia  = []

-- COMENTÁRIOS

-- cria lista vazia

-------------------------------------------------------------------------------------------------------------------------------
-- A.2

quiksort :: (Ord a) => [a] -> [a]
quiksort [] = []
quiksort (x:xs) = quiksort [y | y <- xs, y <= x] ++ [x] ++ quiksort [y | y <- xs, y > x]

-- COMENTÁRIOS

-- ordena lista
-- concatena em 3 partes a menor ++ media ++ maior

------------------------------------------------------------

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : (partition n (drop n xs))

-- COMENTÁRIOS

-- parte a lista em listas eNe elementos de forma recursiva
-- take os xis elementos e chama a funcao enquanto lhe faz o 
-- drop para libertar os que ja foram usados

------------------------------------------------------------

addToList :: (Ord a, Foldable t) => a -> t [a] -> [a]
addToList n xs = quiksort (concat xs ++ [n])

-- COMENTÁRIOS

-- concatena a lista com [n] numa lista
-- devolve lista ordenada

------------------------------------------------------------

adicionaAListaDeAlguidares :: (Ord a, Foldable t) => Int -> a -> t [a] -> [[a]]
adicionaAListaDeAlguidares maxT element xs = partition maxT (addToList element xs)

-- COMENTÁRIOS

-- Estrategia foi particionar a lista em listas de eNe pedidos (maxT) adicionar o 
-- elemento ah lista e devolver ordenada de forma crescente

------------------------------------------------------------
-- STUFF

{-
addToList2 max n xs
    | length xs >= 0 = quiksort (concat xs ++ [n])
    | otherwise = listaDeAlguidaresVazia ++ [n]
-}

-------------------------------------------------------------------------------------------------------------------------------
-- A.3

exists :: Eq t => t -> [t] -> Bool
exists _ [] = False
exists n (x:xs)
    | n == x = True
    | otherwise = exists n xs

-- COMENTÁRIOS

-- verifica de forma recursiva se n existe em xs !!
-- True caso exista

------------------------------------------------------------

elemListaDeAlguidares :: Eq t => t -> [[t]] -> Bool
elemListaDeAlguidares n xs =  or (map (exists n) xs)

-- COMENTÁRIOS

-- aplica a funcao auxiliar exists ao map
-- o resultado eh uma lista de True E/OU False !!
-- OR -> devolve True caso exista pelo menos um True em toda a lista

------------------------------------------------------------
-- STUFF

-- elemListaDeAlguidares2 n xs = (exists n (concat xs))

-------------------------------------------------------------------------------------------------------------------------------
-- A.4

delete :: Eq a => a -> [a] -> [a]
delete n xs = [ x | x <- xs, x /= n ]

-- COMENTÁRIOS

-- devolve uma lista nova sem o eNe
-- excluido pela condicao x /= n

------------------------------------------------------------

removerDaListaDeAlguidares :: Eq a => a -> [[a]] -> [[a]]
removerDaListaDeAlguidares n xs =  map (delete n) xs

-- COMENTÁRIOS

-- devolve uma lista de listas nova sem o eNe que pudesse estar nas listas
-- o map aplica as listas dentro da lista

------------------------------------------------------------
-- STUFF

-- removerDaListaDeAlguidares2 n xs = [ delete n x | x<-xs] -> lista em compreensao

-------------------------------------------------------------------------------------------------------------------------------
-- A.5

concatWithFold :: Foldable t => t [a] -> [a]
concatWithFold xs = foldr (++) [] xs

-- COMENTÁRIOS

-- concatena as listas todas numa unica lista

------------------------------------------------------------

fromList :: Ord a => Int -> [a] -> [[a]]
fromList maxT xs = partition maxT (quiksort (concatWithFold (partition maxT xs )))

-- COMENTÁRIOS

-- particiono com maxT a lista
-- concat com concatWithFold
-- sort com quiksort
-- volto a particionar com maxT

-------------------------------------------------------------------------------------------------------------------------------
-- A.6

mapListaDeAlguidares :: (Ord a1, Foldable t) => Int -> (a2 -> a1) -> t [a2] -> [[a1]]
mapListaDeAlguidares maxT f xs =  partition maxT( quiksort( map (f) (concat xs)))

-- COMENTÁRIOS

-- concat da lista
-- aplico funcao f (*2) e faco map
-- sort com quiksort
-- particiono com maxT a lista

-------------------------------------------------------------------------------------------------------------------------------
--
-- B. KEY-VALUE STORE
--
-------------------------------------------------------------------------------------------------------------------------------
-- B.1

createFastCache :: Int -> [a] -> [b] -> [[(a, b)]]
createFastCache maxT xs ys = partition maxT (zip xs ys)

-- COMENTÁRIOS

-- zip dos elementos de xs e ys
-- particiono com maxT a lista

-------------------------------------------------------------------------------------------------------------------------------
-- B.2

fastGet :: (Foldable t, Eq a1) => t [(a1, a2)] -> a1 -> [a2]
fastGet xs a = [ y | (x,y) <- (concat xs), x == a ]

-- COMENTÁRIOS

-- uso concat para tirar das listas de listas e ficar com so tuplas dentro de lista
-- verifico se o fst == a, se for devolvo y
-- ex: 
-- [[(10,'a'),(20,'b')],[(20,'c'),(30,'d')],[(50,'e')]] -> [(10,'a'),(20,'b'),(20,'c'),(30,'d'),(50,'e')] -> "a"

------------------------------------------------------------
-- STUFF

{-
fastGet (x:xs) n
    | n == fst (head(simplifyList xs)) = concatWithFold resultList
    | otherwise = fastGet (x:xs) n
-}

-------------------------------------------------------------------------------------------------------------------------------




