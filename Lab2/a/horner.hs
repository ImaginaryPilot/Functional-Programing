evalPoly :: [Integer] -> Integer -> Integer
evalPoly xs n = foldl (\a b -> a*n + b) 0 xs