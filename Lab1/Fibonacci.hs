fib::Integer->[Integer]
fib n = f 1 2 []
    where
        f a b xs
            | b > n = xs++[a]
            | otherwise = f b (a+b) (xs++[a])

numberOfFiboSums::Integer->Integer
numberOfFiboSums n = h n (fib n)
    where
        h 0 _ = 1
        h _ [] = 0
        h k (f:fs)
            | f > k = 0
            | otherwise = (h (k - f) fs) + (h k fs)