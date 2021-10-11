------------------------------------------------------------------------------------------------------------------------------
--
-- 4th Trabalho Individual de Principios de Programacao 2020/21
-- @author  -> Marcio Andre Goncalves Pereira Pires Moreira
-- Aluno nr -> 41972
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- CREATE DATA & TYPE
--
-------------------------------------------------------------------------------------------------------------------------------
-- 1

-- modulo de eleicoes com os tipos e funcoes que lhe pertencem
module Eleicoes (
    Candidato,
    Estado,
    Nacao,
    criaNacao,
    obterEstado,
    adicionaVotosEstado,
    adicionaVotosNacao,
    vencedorEstado,
    vencedorEleicao 
    ) where

-- tipo de candidato criado
data Candidato = A | B deriving (Eq, Show) 

-- construtor de tipo Estado 
data Estado = Estado { nome :: String  
                     , peso :: Int
                     , votosA  :: Int
                     , votosB  :: Int
                     } 

-- tipo de Nacao representada por um conjunto de Estados
type Nacao = [Estado] 

-------------------------------------------------------------------------------------------------------------------------------
-- 2

criaNacao :: [(String, Int)] -> [Estado]
criaNacao [] = []
criaNacao ((x,y): xs) =  Estado { nome = x, peso = y , votosA = 0, votosB = 0} : (criaNacao xs)

-- COMENTÁRIOS

-- cria uma nacao com uam lista de pares (estados) e inicializa os votos dos candidatos a 0

-------------------------------------------------------------------------------------------------------------------------------
-- STUFF
{- 

getTupla estado = (nome estado, peso estado)

--instance Show Estado where
    --show (Estado nome peso votosA votosB ) = ("("++nome ++ "," ++ show peso ++ "," ++ show votosA ++ "," ++ show votosB ++ ")")
   
--instance Eq Estado where
--    _ == _ = True
--    _ /= _ = False

-}

-------------------------------------------------------------------------------------------------------------------------------
-- 3

nomeEstado :: Estado -> String
nomeEstado (Estado nome peso votosA votosB) = nome -- devolve o nome do estado

obterEstado :: [Estado] -> String -> Estado
obterEstado xs str = head (filter (\(Estado nome peso votosA votosB) -> nome == str) xs)

obterEstadoAlt :: [Estado] -> String -> [Estado]
obterEstadoAlt xs str  = [ x | x <- xs, (nomeEstado x) == str ] -- funcao alternativa com lista em compreensao

-- COMENTÁRIOS

-- usar filter para comparar o nome do estado com a str da lista de estados
-- devolve o head que tem o estado

-------------------------------------------------------------------------------------------------------------------------------
-- 4

converteNacao :: (String, Int, Int, Int) -> Estado
converteNacao (x,y,z,w) =  Estado { nome = x, peso = y , votosA = z, votosB = w } -- funcAux que cria variaveis para somar valores dos votos

adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado estado numA numB = converteNacao( nome estado, peso estado, (votosA estado + numA), (votosB estado + numB)) 

-- COMENTÁRIOS

-- Usa a funcAux para somar aos votos os valores passados nos argumentos

-------------------------------------------------------------------------------------------------------------------------------
-- 5

fst_Linha :: (a,b,c) -> a
fst_Linha (x,_,_) = x    -- devolve o 1º

snd_Linha :: (a,b,c) -> b
snd_Linha (_,y,_) = y    -- devolve o 2º

third :: (a,b,c) -> c
third (_,_,z) = z        -- devolve o 3º

compara :: Estado -> [(String, Int, Int)] -> Estado
compara estado [] = estado
compara estado (x:xs) = if nome estado == fst_Linha x then adicionaVotosEstado estado (snd_Linha x) (third x) else compara estado xs

-- COMENTÁRIOS

-- compara o estado em cada (x:xs) caso seja == entao adicionaVotosEstado else chama recursivamente o proximo (x:xs)

adicionaVotosNacao :: [Estado] -> [(String, Int, Int)] -> [Estado]
adicionaVotosNacao [] _ = [] 
adicionaVotosNacao (x:xs) ys =  (compara x ys): (adicionaVotosNacao xs ys) 

-- COMENTÁRIOS

-- compara (x:xs) de cada nacao com a lista de dados (Estados e votos) e com o compara adiciona os votos
-- chama recursivamente a funcao para verificar ate ao fim da lista dada

-------------------------------------------------------------------------------------------------------------------------------
-- STUFF
{-

unConverte  :: Estado -> (String, Int, Int, Int)
unConverte  (Estado x y z w) = (x,y,z,w)

convertEmEstado :: (String, Int, Int) -> Estado
convertEmEstado  (x,z,w) = (Estado x 0 z w)

unConverteTodos :: [Estado] -> [(String, Int, Int, Int)]
unConverteTodos xs = map unConverte xs

unConverteTodos xs = [(unConverte x)| x <- xs]

adicionaVotosNacao ((Estado n peso votosA votosB):xs) ((x,y,z):ws)=[converteNacao (n,peso,votosA+y,votosB+z)| w<-ws, a<-(xs), fst3 w == n]

soma :: Nacao -> []
filter (1st == 1st ) ()
soma (x:xs) ys = (first_Linha x, snd_Linha x + snd_Linha ys, third x + third ys)
soma ((x,y,z,w):xs) ((x1,z1,w1):ys) = (x,y,(z+z1),(w+w1)) 

soma :: (Num t1, Num t2) => [(t4, t3, t2, t1)] -> [(t, t2, t1)] -> (t4, t3, t2, t1) -- sai uma soma de tuplas dentro de uma lista
soma [][]= []
soma [] ((x1,z1,w1):ys) = (x1,0,z1,w1) 
soma ((x,y,z,w):xs) [] = (x,y,z,w)

soma2 [] xs = []
soma2 nacao xs
    | nome (n:nacao) == nome ((converte x):xs) = (nome peso (votosA n + votosA n) (votosB n + votosB n))
    | otherwise = soma2 (n:nacao) ((converte x):xs) :xs

adicionaVotosNacao ((x,y,z,w):xs) ((x1,z1,w1):ys) = [ converteNacao(x,y,(z+z1),(w+w1)) | a<-xs , x == x1 ]

--adicionaVotosNacao :: Nacao -> [(String,Int,Int)] -> Nacao
--adicionaVotosNacao (x:xs) ys = map (compara (Estado x) ys) xs
--adicionaVotosNacao (x:xs) ys =  filter ((compara x ys): (adicionaVotosNacao xs ys) ) ys

-}
--
-------------------------------------------------------------------------------------------------------------------------------
-- 6

vencedorEstado :: Estado -> Maybe Candidato
vencedorEstado estado   
    | (votosA estado) > (votosB estado) = Just A
    | (votosB estado) > (votosA estado) = Just B
    | otherwise = Nothing

-- COMENTÁRIOS

-- se votosA forem mais que votosB entao vencedor == A
-- caso contrario vencedor == B
-- outro caso == empate/Nothing

-------------------------------------------------------------------------------------------------------------------------------
-- 7

-- devolve um candidato com funcAuxiliar
funcAux :: Maybe Candidato -> Candidato
funcAux x
    | x == Just A = A -- se x for Just A entao == A caso contrario == B
    | otherwise = B

-- recebe uma nacao e devolve uma lista de canditato vencedor em cada estado e o respectivo peso de (x:xs)
lista :: Nacao -> [(Int,Candidato)]
lista (xs) = [(peso x,funcAux(vencedorEstado x)) | x<-xs ] 

contagemVotos :: (Int,Int) -> [(Int,Candidato)] -> (Int,Int)
contagemVotos (y,z) [] = (y,z)
contagemVotos (y,z) (x:xs)
    | snd x == A = contagemVotos (y+fst x, z) xs  -- se valor da tupla de (int, candidato) == A, somar valor de y com fst x  
    | snd x == B = contagemVotos (y, z+fst x) xs -- se valor da tupla de (int, candidato) == B, somar valor de z com fst x 
    | otherwise = contagemVotos (y,z) xs -- caso contrario empate


vencedorEleicao :: Nacao -> Maybe Candidato
vencedorEleicao (xs) 
    | (fst (contagemVotos (0,0) (lista xs))) > (snd (contagemVotos (0,0) (lista xs))) = Just B  -- se o primeiro da tupla de contagem de votos > entao o vencedor == B
    | (fst (contagemVotos (0,0) (lista xs))) < (snd (contagemVotos (0,0) (lista xs) )) = Just A -- se o segundo da tupla de contagem de votos > entao o vencedor == A
    | otherwise = Nothing -- caso contrario == Empate

------------------------------------------------------------------------------------------------------------------------------
-- STUFF
{-

ganha quem tiver mais peso
ver estado a estado com o vencedor do estado e ver para quem vai o peso se é A ou B usar vencedor estado
arranjar forma de guardar valor de A e B

-}
--
-------------------------------------------------------------------------------------------------------------------------------
-- 8

-- dois estados com o mesmo peso sao iguais assim como se o vencedor for o mesmo
instance Eq Estado where
    x == y = (peso x == peso y) && (vencedorEstado x == vencedorEstado y)

-------------------------------------------------------------------------------------------------------------------------------
-- 9

-- instancia de show para o tipo Estado que devolve uma representacao textual do tipo ("personalisada")
instance Show Estado where
    show (Estado nome peso votosA votosB ) = (nome ++ " " ++ show peso ++ " " ++ show votosA ++ " " ++ show votosB )

-------------------------------------------------------------------------------------------------------------------------------

