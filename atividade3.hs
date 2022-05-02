import Data.Char


pertence _ [] = False
pertence e (x:xs)   | e == x = True
                    | otherwise = pertence e xs

intercessao _ [] = []
intercessao [] _ = []
intercessao (x:xs) ys   | pertence x ys = x : r--pertence x (y:ys) = x O caso base já é TRUE
                        | otherwise = r
                where r = intercessao xs ys

somaListas [] _ = []
somaListas _ [] = []
somaListas (x:xs) (y:ys) = (x+y): somaListas xs ys

pot2 0 = [1]
pot2 1 = [2]
pot2 n = 2^n : pot2 (n-1)

pot2' 0 = [1]
pot2' 1 = [2]
pot2' n = pot2' (n-1) ++ [2^n]

menor [x] = x
menor (x:xs)    | x < menor xs = x
                | otherwise = menor xs

ordenar [] = []
ordenar (x:xs) = [menor (x:xs)] ++ ordenar (removerElem (menor(x:xs)) (x:xs)) -- algo está errado


ins e (x:xs) = ordenar ([e] ++ (x:xs))

interlacao [] ys = ys
interlacao xs [] = xs
interlacao (x:xs) (y:ys)        | x < y = x:interlacao xs (y:ys)
                                | otherwise = y:interlacao (x:xs) ys

removerElem _ [] = []
removerElem p (x:xs)    | p == x = xs
                        | otherwise = x:removerElem p xs

enesimo n (x:xs)        | n == 1 = x
                        | otherwise = enesimo (n-1) xs

repetir n e     | n == 1 = [e]
                | otherwise = e:repetir (n-1) e

digito 0 = '0'
digito 1 = '1'
digito 2 = '2'
digito 3 = '3'
digito 4 = '4'
digito 5 = '5'
digito 6 = '6'
digito 7 = '7'
digito 8 = '8'
digito 9 = '9'
intString n = inv (intString' n)
        where
                intString' 0 = []
                intString' n = digito (rem n 10): intString' (div n 10)

inv [] = []
inv (x:xs) = inv xs ++ [x] --buscar entender esse ++

nultimos 0 _ = []
nultimos _ [] = []
nultimos n (x:xs) = nprimeiros (n) (inv(x:xs)) --sincerametne não entendo

nprimeiros 0 _ = []
nprimeiros _[] = []
nprimeiros n (x:xs) = x : nprimeiros(n-1) xs

degito '0' = 0
degito '1' = 1
degito '2' = 2
degito '3' = 3
degito '4' = 4
degito '5' = 5
degito '6' = 6
degito '7' = 7
degito '8' = 8
degito '9' = 9
stringNum [] = 0
stringNum (x:xs) = (degito x)*10^(length xs) + (stringNum xs) -- n é um contador que vai de 1 até acabar; 1 -> 2 -> 3 -> 4..
-- tem muita coisa errada aqui

minusculas [] = []
minusculas (x:xs) = [toLower x] ++ minusculas xs