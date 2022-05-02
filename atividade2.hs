somaPares 0 = 0
somaPares n | mod n 2 == 0 = n + somaPares (n-1)
            | otherwise = somaPares (n-1)

sumaQuadrado n m    | m == 0 = 0
                    | otherwise = 2^m*n + sumaQuadrado n (m-1)

numPerfeito n   | numPerfeito'n (n-1) == n = True
                | otherwise = False

numPerfeito' 6 6
numPerfeito'num ctr     | rem num ctr == 0 = ctr + e
                        | otherwise = r
                where r = numPerfeito' num (ctr-1)

fat' n  | n == 0 = 1
        | otherwise = n * fat' (n-1)

fat'' n = fataux 1 n

fataux n m      | n <= m = m * fataux (n+1) m
                | otherwise = 1

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1) 

--achar mais dois jeitos de resolver fibonacci

multiComSoma n m    | m == 1 = n
                    | otherwise = n + multiComSoma n (m-1)
primo 3 = True
primo 5 = True
primo 7 = True
primo 11 = True
primo n | mod n 2 == 0 = False
        | mod n 3 == 0 = False
        | mod n 5 == 0 = False
        | mod n 7 == 0 = False
        | mod n 11 == 0 = False
        | otherwise = True