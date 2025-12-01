trailingZeros :: [Integer]
trailingZeros = h 2
    where
        h n
            | n == 2 = 1 : h (n+1)
            | odd n = 0 : h (n+1) 
            | otherwise = (trailingZeros!!(div (n-3) 2) + 1) : h (n+1)