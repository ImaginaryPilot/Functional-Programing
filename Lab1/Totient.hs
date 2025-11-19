divisors:: Integer -> [Integer]
divisors n = 2:[3,5..n]

isPrime:: Integer -> Bool
isPrime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = null [d | d<-[3,5..floor(sqrt(fromIntegral n))], mod n d == 0] 

distinctPrimes:: Integer -> [Integer]
distinctPrimes n = [p | p <- divisors (floor(sqrt(fromIntegral n))), isPrime p]

totient::Integer->Integer
totient n = h n (distinctPrimes n) n
    where
        h n [] k = floor(k)
        h n xs k = h n (tail(xs)) (k*(1-(1/head(xs)))) 