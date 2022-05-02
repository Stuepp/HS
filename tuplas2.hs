{-1 Declare uma função que receba uma lista de duplas [{a,b}], e retorne uma lista
com os primeiro elemento de cada dupla [a]-}

primeiros [] = []
primeiros (x:xs) = fst x : primeiros xs

{- 2 Declare uma função que receba uma lista de duplas e retorne uma lista com a soma
dos elementos de cada dupla.-}

--sumarDuplas :: [(Int),(Int)] -> [Int]

somarDuplas [] = []
somarDuplas (x:xs) = fst x + snd x : somarDuplas xs

{- 4 Declare uma função que receba um elemento e, e uma lista de duplas, a função deve
procurar a primeira dupla cujpo elemento da dupla seja igual ao parametro e.
A função deve retornar o segundo elemento dessa dupla.-}

procurar _ [] = ""
procurar n (x:xs)   | fst x == n = snd x
                    | otherwise = procurar n xs

{- 5 Declare uma função que receba uma lista de duplas e retorne uma lista contendo todas as duplas cujo
primeiro elemento seja menor que o segundo-}

menores [] = []
menores ((x1,x2):xs)    | x1 < x2 = (x1,x2) : menores xs
                        | otherwise = menores xs

{- 7 Declare uma função que receba um valor v e uma lista, a função deve retornar uma dupla de listas,a primeira lista deve conter
os elementos que são menores ou iguais a v e a segunda lista deve retornar os elementos maiores que v-}

separar _ [] = ([],[])   -- [a]
separar e i@(x:xs) = (menores' e i,maiores e i) -- ([a],[a])

menores' e [] = []
menores' e (x:xs)   | e > x = x : menores' e xs
                    | otherwise = menores' e xs

maiores e [] = []
maiores e (x:xs)    | e < x = x : maiores e xs
                    | otherwise = maiores e xs

{- 9 Declare uma função que receba uma lista de duplas [(x,y)], e retorne uma lista com o inverso de cada dupla,ou seja [(y,x)]-}

inverso [] = []
inverso ((x1,x2):xs) =  (x2,x1) : inverso xs

{- 10 Declare uma função que receba uma lista de duplas, e retorne uma lista indicando se os elementos são iguais ou não (True/False)-}

simetrico [] = []
simetrico ((x1,x2):xs)  | x1 == x2 = True : simetrico xs
                        | otherwise = False : simetrico xs

{- 11 Declare uma função que recebe 2 listas de duplas [(a,b)] [(c,d)], e retorna a composição da primeira com a segunda lista,
na forma [(a,d)], onde a  é o primeiro elemento da primeira lista, e c é o segundo elemento da segunda lista-}

comporDuplas [] _ = []
comporDuplas _ [] = []
comporDuplas ((x1, _):xs) ((_,y2):ys) = (x1, y2) : comporDuplas xs ys


{- 3 Declare uma função que receba uma lista de duplas e retorne uma dupla com o maior e o menor elemento dela-}

maior [x] = x
maior (x:y:xs) = if x > y then maior (x:xs) else maior (y:xs)

menor [x] = x
menor (x:y:xs) = if x < y then menor (x:xs) else menor (y:xs)

--maiorMenor [] = []
maiorMenor l@(x:xs) = (maior l , menor l)

{-Declare uma função que receba um número inteiro, e retorna uma lista de duplas de inteiros distintos (x,y)
tal que 1 <= x, y <= i
pares 3 = [(1, 2),(1, 3),(2, 1),(2, 3),(3, 1),(3, 2)]-}

pares e = --((e/e),(e/e)+(e/e)),pares e