divisors:: Integer -> [Integer]
divisors n = 2:[3,5..n]

isPrime:: Integer -> Bool
isPrime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = null [d | d<-[3,5..floor(sqrt(fromIntegral n))], mod n d == 0] 

sumPrimesTo:: Integer -> Integer
sumPrimesTo n = sum[p | p <- divisors n, isPrime p]