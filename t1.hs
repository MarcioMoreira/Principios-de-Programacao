-- Marcio Andre Goncalves Pereira Pires Moreira
-- Aluno - 41972
--
-- Primeiro Trabalho Tp de PP
--
--
-- A. Deteccao de Plagio

frequencias :: Eq a => [a] -> [Int]
frequencias xs = [ (length.filter (==x)) xs | x<-xs]

-------------------------------------------------------------------------------------------------------------------------------
--
-- B. Pequenas Palavras

vogalY :: [Char]
vogalY =['a','e','i','o','u','y']

pequenasPalavras :: [[Char]]
pequenasPalavras =  [[a,b,c] | a<-['a'..'z'] , b<-['a'..'z'], c<-['a'..'z'], elem a vogalY || elem b vogalY || elem c vogalY ]

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
getIndiceLista xs = [ x| x <- xs]

--numDeLados numAndares evitarAndar = [ (numAndares)++ if numAndares >= evitarAndar then numAndares == numAndares+1 else numAndares == numAndares]

legendaCampainha :: (Num a, Enum a, Eq a, Show a) =>  a -> a -> [([Char], b)] -> [[Char]]
legendaCampainha numAndares evitarAndar xs = [(show(andar) ++ fst x)  |   andar <- [1..(numAndares)] , x<-xs, andar /= evitarAndar  ]






