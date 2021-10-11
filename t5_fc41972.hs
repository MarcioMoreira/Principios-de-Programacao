------------------------------------------------------------------------------------------------------------------------------
--
-- 5th Trabalho Individual de Principios de Programacao 2020/21
-- @author  -> Marcio Andre Goncalves Pereira Pires Moreira
-- Aluno nr -> 41972
--
-------------------------------------------------------------------------------------------------------------------------------
--
-- IO STUFF - STACKING & FILTERING
--
-------------------------------------------------------------------------------------------------------------------------------

import System.Environment -- get Args
import System.IO
import System.Exit
import Control.Monad
import Data.List 
import System.Directory

-- Gives filtered lines
getLinhasValidas :: [Char] -> [[Char]] -> IO ()
getLinhasValidas str xs = mapM_ putStrLn [ x | x<-xs, isInfixOf str x]

limpaPop xs = mapM_ init [ x | x<-xs ]

-- The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', terminates the program
-- successfully.
exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

-- List
lista :: [a]
lista = []

-- Prints a list IO with ', '
printList :: [String] -> IO ()
printList [] = return ()
printList [x] = putStr x
printList (x:xs) = do
    putStr x
    putStr ", "
    printList xs

-------------------------------------------------------------------------------------------------------------------------------

main = do
    [file] <- getArgs  -- le ficheiro
    str    <- readFile file  
    let linhas = lines str
    mapM_ putStrLn linhas 
    appendFile "stack.txt" $ "" -- inicializa stack vazia
    putStrLn ("\nFiltering: " )

    forever $ do
        putStr ("> " )
        input <- getLine  -- recebe input

        appendFile "stack.txt" $ input ++ "\n" -- faz append do input na stack ja inicializada
        str1    <- readFile "stack.txt" -- le da stack
        
        let linhas1 = lines str1
        let lista2 = lista ++ lines str1 -- cria lista com o que vem da stack (faz parse)

        putStr "\n"
        getLinhasValidas (last linhas1) linhas -- linhas validadas pelo filtro
        
        putStr ("\nFiltering: " )
        printList linhas1  -- print da lista dos filtros
        
        let exitSuccess = exitWith ExitSuccess

        if (input == "pop" && length lista2 == 0)  -- STUFF COM OS IOs A ATROFIAR
           then exitSuccess
           else return ()-- lista2 = init (init lista2) 
           --else return () -- putStr ("\n\nFAZ POP\n" ) fazer init (init lista2) o POP entra logo tem de sair mais o anterior
 {-     
        if (input == "pop" && length lista2 > 0)  -- STUFF COM OS IOs A ATROFIAR
           then lista2 = init (init lista2) 
           else return ()
        -- putStr ("\n\nFAZ POP\n" ) fazer init (init lista2) o POP entra logo tem de sair mais o anterior
-}

        if input == "q" || input == "quit" || input == "exit" -- extra para parar o programa
           then exitSuccess 
           else return ()
        putStrLn ""
        
    removeFile "stack.txt" -- remove o ficheiro no fim

-------------------------------------------------------------------------------------------------------------------------------

-- STUFF

{-

printLista [] = putStr ""
printLista [x] = putStrLn x
printLista (x:xs) = do
    let element = x
    putStrLn element
    printLista xs

readTable = do
    s <- readFile "stack.txt" 
    let content = lines s
    let removeEnd = init(content)
    let removeHead = tail(removeEnd)
    printLista removeEnd

limpaInit :: [a] -> [a]
limpaInit [] = return ()
limpaInit [x] = putStr x
limpaInit (x:xs) = init xs

-}
