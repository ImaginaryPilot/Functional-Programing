import Data.Char

encode::Int->Char->[Char]
encode n c 
    | c >= 'A' && c <= 'Z' = [chr ((mod (ord 'Z' - ord 'A' - n + 1) 26) + ord 'A')]
    | otherwise = [c]

cipherEncode::Int->String->String
cipherEncode n xs = h n xs "" n
    where
        h n [] a k = a
        h n xs a k = h n (tail(xs)) (a++(encode k (head(xs)))) (mod (k+n) 26)
