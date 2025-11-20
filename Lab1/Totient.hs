divisors:: Integer -> [Integer]
divisors n = 2:[3,5..n]

isPrime:: Integer -> Bool
isPrime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = null [d | d<-[3,5..floor(sqrt(fromIntegral n))], mod n d == 0] 

distinctPrimes:: Integer -> [Integer]
distinctPrimes n = [p | p <- divisors (div n 2), isPrime p] ++ [n]

divide::Integer->Integer->(Integer, Integer)
divide m n = h m n 0
    where
        h m n count
            | mod m n == 0 = h (div m n) n (count+1)
            | otherwise = (m, count)

primeFactors::Integer->[(Integer,Integer)]
primeFactors n = h n (distinctPrimes n) []
    where
        h 1 ds xs = xs
        h n ds xs 
            | snd k == 0 = h n (tail ds) xs 
            | otherwise = h (fst k) (tail ds) (xs++[(i,snd k)])
            where
                i = head (ds) 
                k = divide n i 

totient::Integer->Integer
totient n = product [p^(e-1) * (p-1) | (p,e) <- primeFactors n]
