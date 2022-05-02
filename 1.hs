maior a b = if a > b then a else b

negacao x = if x then False else True

maior3 a b c = maior (maior a b) c

pot a 0 = 1
pot a e = a * pot a (e-1)

fat 0 = 1
fat n = n * fat(n-1)

maximoDivisorComum a b  | a == b = a
                        | a > b = maximoDivisorComum (a - b) b
                        | otherwise = maximoDivisorComum a (b - a)