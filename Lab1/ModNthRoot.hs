isNthRootOfUnity:: Integer -> Integer -> Integer -> Bool
isNthRootOfUnity x n m = x^n `mod` m == 1