chebyshev :: Integer -> Integer

chebyshev n = h n 1 0
    where
        h 0 a b = 0
        h 1 a b = 2*a
        h n a b = h (n - 1) (4*a - b) a