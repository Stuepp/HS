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

{- 3 Declare uma função que receba uma lista de duplas e retorne uma dupla com o maior e o menor elemento dela-}

maior [x] = x
maior (x:y:xs) = if x > y then maior (x:xs) else maior (y:xs)

menor [] = []
menor (x:xs)    | x < menor xs = x
                | otherwise = menor xs


maiorMenor [] = []
maiorMenor l@(x:xs) = (maior l , menor l)

{- 5 Declare uma função que receba uma lista de duplas e retorne uma lista contendo todas as duplas cujo
primeiro elemento seja menor que o segundo-}

menores [] = []
menores ((x1,x2):xs)    | x1 < x2 = (x1,x2) : menores xs
                        | otherwise = menores xs

{-Declare uma funções que receba como parâmetro uma String e retorne uma dupla de Strings, a primeira String
deve conter letras maiúsculas e a segunda as letras minúsculas. Os caracteres que não forem letras devem ser ignorados
(Olhas as funções isLower isUpper do módulo Data, Char)-}

--maiuscMinusc (x:xs) 

{- 7 Declare uma função que receba um valor v e uma lista, a função deve retornar uma dupla de listas,a primeira lista deve conter
os elementos que são menores ou iguais a v e a segunda lista deve retornar os elementos maiores que v-}

separar 0 _ = _
separar e [] = []
separar e i@(x:xs) = menores' e i : maiores e i

menores' e (x:xs)   | e > x = x : menores' e xs
                    | otherwise = menores xs

maiores e (x:xs)    | e < x = x : maiores e xs
                    | otherwise = maiores xs

{- 9 Declare uma função que receba uma lista de duplas [(x,y)], e retorne uma lista com o inverso de cada dupla,ou seja [(y,x)]-}

inv [] = []
inv (x:xs) = inv xs ++ [x]

inverso [] = []
inverso ((x1,x2):xs) = inv (x1,x2) : inverso xs

{- 10 Declare uma função que receba uma lista de duplas, e retorne uma lista indicando se os elementos são iguais ou não (True/False)-}

simetrico ((x1,x2):xs)  | x1 == x2 = True : simetrico xs
                        | otherwise = False : simetrico xs

{- 11 Declare uma função que recebe 2 listas de duplas [(a,b)] [(c,d)], e retorna a composição da primeira com a segunda lista,
na forma [(a,d)], onde a  é o primeiro elemento da primeira lista, e c é o segundo elemento da segunda lista-}

comporDuplas [] _ = []
comporDuplas _ [] = []
comporDuplas ((x1, _):xs) ((_,y2):ys) = (x1, y2) : comporDuplas xs ys

{-Declare uma função que recebe um número inteiro, e retorna uma lista de duplas de inteiros distintos (x,y) tal que 
1 <= x, y <= i-}