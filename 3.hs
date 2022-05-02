-- () -> forma de armazenar tuplas
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = [] --1º lista vazia
zip' []_ = [] --2º lista vazia
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xys) = (x:fst (unzip' xys), y:snd (unzip' xys))
    where
        (xs, ys) = unzip' xys