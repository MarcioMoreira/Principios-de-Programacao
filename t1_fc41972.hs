-------------------------------------------------------------------------------------------------------------------------------
--
-- 1st Trabalho Individual de Principios de Programacao 2020/21
-- @author  -> Marcio Andre Goncalves Pereira Pires Moreira
-- Aluno nr -> 41972
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- A. Deteccao de Plagio

frequencias :: Eq a => [a] -> [Int]
frequencias xs = [ (length.filter (==x)) xs | x<-xs] 

-- COMENTÁRIOS

-- corre a lista e usa filter com o length calcula o nr de repeticoes em que x == x:xs

-------------------------------------------------------------------------------------------------------------------------------
--
-- B. Pequenas Palavras

vogalY :: [Char]
vogalY =['a','e','i','o','u','y']

pequenasPalavras :: [[Char]]
pequenasPalavras =  [[a,b,c] | a<-['a'..'z'] , b<-['a'..'z'], c<-['a'..'z'], elem a vogalY || elem b vogalY || elem c vogalY ]

-- COMENTÁRIOS

-- compoe palavras com 3 caracteres gerados pelos ciclos
-- elem fazem com que as palavras cumpram o requisito de ter pelos menos uma vogal pertencente a lista volgaY

-------------------------------------------------------------------------------------------------------------------------------
--
-- C. Campainhas de Predios

ocidentEvita :: Num p => p
ocidentEvita = 13

orientEvita :: Num p => p
orientEvita = 4

portas :: [Char]
portas = ['A'..'Z']

portas2 :: [[Char]]
portas2 = ["Dto", "Esq", "Cent"]

portas3 :: [[Char]]
portas3 = ["Norte", "Sul", "Este", "Oeste"]

getAndar :: [(a, a)] -> [a]
getAndar xs = [ snd x | x<-xs ]

getPorta :: [(a, b)] -> [a]
getPorta xs =  [ fst x | x<-xs ]

getIndiceLista :: [a] -> [a]
getIndiceLista xs = [ x | x <- xs]

legendaCampainha :: (Num a, Ord a, Enum a, Show a) =>p -> a -> [([Char], a)] -> [[Char]]
legendaCampainha numAndares evitarAndar ls = concat [[show x ++ y | y <-[ fst(l)], x <-[ 1..(if evitarAndar <= snd(l) then snd(l)+1 else snd(l)) ], x/= evitarAndar] | l<-ls]

-- COMENTÁRIOS

-- contatena x com y e faz show
-- y == fst (l:ls)
-- x == fst (l:ls)
-- evitarAndar <= snd(l) controla o salto de andar a evitar com => x/= evitarAndar
-- gera lista de listas usa-se o concat no final

-------------------------------------------------------------------------------------------------------------------------------
-- STUFF
--
--numDeLados numAndares evitarAndar = [ (numAndares)++ if numAndares >= evitarAndar then numAndares == numAndares+1 else numAndares == numAndares]

--geraPortas n m [] = []
--geraPortas n m ((porta,maxAndares):ls) = [ show x ++ y | y <-[porta], x <-[1..(if m<=maxAndares then maxAndares+1 else maxAndares)] ,x/=m  ] ++ geraPortas n m ls

--legendaCampainha :: (Num a, Enum a, Eq a, Show a) =>  a -> a -> [([Char], b)] -> [[Char]]
--legendaCampainha numAndares evitarAndar xs = [(show(andar) ++ fst x)  |   andar <- [1..(numAndares)] , x<-xs, andar /= evitarAndar  ]