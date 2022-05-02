funcao2 [] = []
funcao2 xs = xs : funcao2 (nprimeiros (length (xs)-1) xs)

nprimeiros 0 _ = []
nprimeiros _[] = []
nprimeiros n (x:xs) = x : nprimeiros(n-1) xs

troca (x:xs) 0 = (x:xs)
troca [] n = []
troca (x:xs) n = troca (xs ++ [x]) (n-1)

--zip3 :: [a] -> [b] -> [(a,b)]
zip'3 _ _ [] = []
zip'3 [] _ _ = []
zip'3 _ [] _ = []
zip'3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip'3 xs ys zs

isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

temVogal [] = []
temVogal (x:xs) = map (\x -> if isVowel x then True else False) (x:xs)

funcao1 _ [] = []
funcao1 [] (y:ys) = (y:ys)
funcao1 (x:xs) (y:ys) | elem x (y:ys) = funcao1 xs (removerElem x (y:ys))
                      | otherwise = y : funcao1 xs ys

removerElem _ [] = []
removerElem p (x:xs)    | p == x = xs
                        | otherwise = x:removerElem p xs

inversoExtIn [] = []
inversoExtIn (x:xs) =  inv (inv x : inversoExtIn (inv xs))

inv [] = []
inv (x:xs) = inv xs ++ [x]

trocaTupla [] = []
trocaTupla(x:[]) = []
trocaTupla (x:x2:xs) = (snd x, fst x2) : trocaTupla (x2:xs)