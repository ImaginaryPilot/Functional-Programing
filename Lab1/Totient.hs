divisors:: Integer -> [Integer]
divisors n = 2:[3,5..n]

isPrime:: Integer -> Bool
isPrime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = null [d | d<-[3,5..floor(sqrt(fromIntegral n))], mod n d == 0] 

factors :: Int -> [Int]
factors n = [d | d <- [1 .. floor(sqrt(fromIntegral n))], mod n d == 0]

distinctPrimes:: Integer -> [Integer]
distinctPrimes n = [p | p <- divisors n, isPrime p]

divide::Integer->Integer->(Integer, Integer)
divide m n = h m n 0
    where
        h m n count
            | mod m n == 0 = h (div m n) n (count+1)
            | otherwise = (m, count)

totient::Integer->Integer
totient n = h n (distinctPrimes n) n
    where
        h n _ k = k
        h n xs k
            | mod j n == 0 = h (fst(i)) (tail(xs)) (k^(snd i)*(j - 1))
            | otherwise = h n (tail(xs)) k
            where 
                j = head(xs)
                i = divide n j
