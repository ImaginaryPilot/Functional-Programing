isNthRootOfUnity:: Integer -> Integer -> Integer -> Bool
isNthRootOfUnity x n m = h x n m 1
    where
        h x 1 m a = mod (a*x) m == 1
        h x n m a
            | n == 2*k = h (mod (x^2) m) k m a
            | n == 2*k + 1 = h (x^2) k m (mod (x*a) m)
            where k = div n 2 
