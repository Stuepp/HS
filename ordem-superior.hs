{-
map
filter
folder / fold
-}
triplo1 [] = []
triplo1 (x:xs) = 3 * x : triplo1 xs

triplo2 [] = []
triplo2 lista = 3*(head lista) : triplo2 (tail lista)

triplo3 [] = []
triplo3 xs = a : triplo3 b
    where a = 3*(head xs)
          b = tail xs

triplo4 xs = [3*x | x <- xs]

triplo5' x = 3 * x
triplo5 xs = [triplo5' x | x <- xs]

triplo6 xs = map(3*) xs

--map :: (a -> b) -> [a] -> [b]

map' f (x:xs) = f x : map' f xs

{-
Função é um valor de primeira ordem:
avaliação parcial

Função de ordem superior:
recebe uma função como argumento
-}

map'' f xs = [f x | x <- xs]

primeirosDuplas xs = map fst xs --ou map (fst) xs basta tomar cuidado com o escopo

maiorDupla xs = map max' xs

max' (a,b) = if a > b then a else b

contaPalavras xs = map length xs
