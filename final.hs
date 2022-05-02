import System.IO
import Data.Char
import Data.List

allNumWords [] = []
allNumWords ((x1,x2):xs) = (x1,words x2) : allNumWords xs

sortLs [] = []
sortLs ((x1,x2):xs) = (x1,sort x2) : sortLs xs

--almagamate ((x1,x2):xs) | elem x2 xs = ([x1],x2) : almagamate xs
--                        | otherwise = (x1,x2) : almagamate xs

main = do putStr "Arquivo: "
          hFlush stdout
          narq <- getLine
          putStrLn ("O Nome digitado foi: " ++ narq)
          texto <- readFile narq
          putStrLn texto
          putStrLn ("O arquivo " ++ narq ++ " possui tamanho " ++ show(length (texto)))
          putStrLn "///"
          putStrLn "a)"
          putStrLn (show (lines texto))
          putStrLn "b)"
          putStrLn (show $ zip [1..] (lines texto))
          putStrLn "c)"
          putStrLn $ show $ allNumWords $ zip [1..] (lines texto)
          putStrLn "d)"
          putStrLn $ show $ sortLs $ allNumWords $ zip [1..] (lines texto)
          putStrLn "e)"

