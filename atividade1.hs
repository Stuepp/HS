ehPar n = if mod n 2 == 0 then True else False

anoIdade year = 2020-year --Descobrir como implementar um getYear

permiteDirigir n | anoIdade n >= 18 = True | otherwise =  False

zeroPosNeg n    | n == 0 = putStrLn "0" 
                | n > 0 = putStrLn "positivo"
                | n < 0 = putStrLn "negativo"

media3 x y z = div (z+y+z) 3 

areaSala x y = x*y

minHoras m = div m 60

dobroOuTriplo n | n > 0 = n*2 
                | n < 0 = n*3

ehAprovado x y z | (x+y+z)/3 >= 7 = putStrLn "aprovado" | otherwise = putStrLn "reprovado"

somaDigitos n = ((div n 100)+((div n 10)-((div n 100)*10))+(n -((div n 10)*10)))