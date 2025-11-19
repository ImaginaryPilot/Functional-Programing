import Data.Char

encode::Int->Char->[Char]
encode n c 
    | c >= 'A' && c <= 'Z' = [chr ((mod (ord c - ord 'A' - n + 26) 26)  + ord 'A')]
    | otherwise = [c]

cipherEncode::Int->String->String
cipherEncode n xs = h n xs "" n
    where
        h n [] a k = a
        h n xs a k 
            | c >= 'A' && c <= 'Z' = h n (tail(xs)) (a++(encode k c)) (mod (k+n) 26)
            | otherwise = h n (tail(xs)) (a++(encode k c)) k
            where c = head(xs)

decode::Int->Char->[Char]
decode n c 
    | c >= 'A' && c <= 'Z' = [chr ((mod (ord c - ord 'A' + n + 26) 26)  + ord 'A')]
    | otherwise = [c]

cipherDecode::Int->String->String
cipherDecode n xs = h n xs "" n
    where
        h n [] a k = a
        h n xs a k 
            | c >= 'A' && c <= 'Z' = h n (tail(xs)) (a++(decode k c)) (mod (k+n) 26)
            | otherwise = h n (tail(xs)) (a++(decode k c)) k
            where c = head(xs)    
