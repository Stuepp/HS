import Data.Char
--1 Usando a função filter declare
--uma lista infinita com os números naturais que são simultaneamente múltiplos de dois e três.

calice = filter(\n -> mod n 2 == 0 && mod n 3 == 0) [1..]

--2 Declare uma função que receba uma lista de Strings e retorne uma lista de duplas onde, o primeiro elemento
--é a String recebida, e o segundo é o tamanho desta String

contaPalavras [] = []
contaPalavras xs = map length xs --falta botar as palavras dentro da resposta

--3 Declare uma função que recebe um número n e retorna pares de valores distintos (x,y), tal que: 1<=x,y<=n.
--Utileze filter na construção.

pares n = filter (\(x,y) -> x /= y) ([(a,b) | a <- [1..n], b <- [1..n]])

--4 Declare uma funçãoq que receba uma lista de caracteres, e retorne a mesma lista porém removendo as letras
--maiúsculas

semMaiusc xs =  filter ((\x -> isUpper x == False)) xs

--5 Declar  e uma função que receba uma lista de listas, e remove toda a ocorrência de lista vazia deta lista de
--listas

semListasVazias xs = filter (\x -> x /= []) xs --tem algum problema

--6 Declare uma função que receba uma lista com pares de inteiros e retorne outra lista que contém a soma de cada par

somaPares xs = map (\x -> fst x + snd x) xs

--7 Declare uma função que receba uma lista de números inteiros e retorna uma dupla de listas ([a],[b]), onde 
--[a] contempla os elementos ímpares,e [b] os elementos pares.

separarParImpar xs = (filter (\x -> mod x 2 == 0) xs, filter (\x -> mod x 2 /= 0) xs)

--8.Declare uma função com comportamento equivalente a função take da biblioteca prelude de Haskell,
--ou seja, deve receber um número inteiro n e uma lista, e retornar os n primeiros desta lista.
--Utilize as funções de ordem superior.

take' 0 (x:xs) = []
take' _ [] = []
--take' z (x:xs) = if z /= 0 then x : take' (z-1) xs else ??

--9.Declare uma função, usando foldr, que receba um lista de valores booleanos e retorne a True
--se todos elementos da lista forem True, a função deve retornar False caso contrário.

--10.Declare uma função que recebe um número inteiro n, e retorna a soma do quadrado
--dos n primeiros números. Defina duas versões desta função, uma utilizando map e outra utilizando fold.

somaQuadrado n = sum (map (\x -> x*x) [1..n])

--11.Declare a sua versão da função length disponível na biblioteca de Haskell Prelude, porém utilizando
--a função de ordem superior foldr.Ex: length' "Programacao Funcional" => 21

plus1 _ y = 1 + y
len'' xs = foldr plus1 0 xs

--12.Declare uma função que recebe uma lista de inteiros, e retorna o menor número inteiro desta lista.
--Defina 2 versões desta implementação, uma utilizando foldr e outra utilizando foldl.

--13.Declare uma função que recebe uma lista de Strings e retorna uma lista de booleanos tal que,
--True é o enésimo elemento da lista de inteiros se o enésimo elemento da lista de Strings tem um número
--de caracteres par, e False caso seja ímpar.

--paridade xs = filter (\x -> True if x /= mod (length x) 2 /= 0 else False) xs