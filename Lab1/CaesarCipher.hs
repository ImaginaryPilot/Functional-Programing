rotateLeft::Int->String->String
rotateLeft n string = drop n string ++ take n string

rotateRight::Int->String->String
rotateRight n string = drop (k-n) string ++ take (k-n) string 
    where k = length(string)

list = ['A'..'Z']

cipherEncode::Int->String->String
cipherEncode n xs = h n xs ""
    where
        h n [] a = a
        h n (x:xs) a = h n (tail xs) a:x
