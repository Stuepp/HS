--[] -> para listas de um único tipo
--(:) -> para listas de multiplos elementos

tamanho :: [a] -> Int
tamanho [] = 0 --pegando o primeiro elemento da lista
tamanho (x:xs) = 1 + tamanho xs
{-demontano a função em elemento x e xs, onde
--x é o atual elemento zero, e xs o resto
--assim excluindo o elemento zero e criando uma nova
--lista com o xs
-}
nprimeiros :: Int -> [a] -> [a]
nprimeiros 0 _ = []
nprimeiros _[] = []
nprimeiros n (x:xs) = x : nprimeiros(n-1) xs

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

pertence e [] = False
pertence e (x:xs) = if e == x then True else pertence e xs

