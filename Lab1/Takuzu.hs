strings::Int->[String]
strings 0 = [""]
strings n = [c : s | c <- "01", s <- strings (n-1), not (hasTriple (c:s))]

hasTriple::String->Bool
hasTriple (a:b:c:xs)
    | a == b && b == c = True
    | otherwise = hasTriple (b:c:xs)
hasTriple _ = False

takuzuStrings::Int->[String]
takuzuStrings n = strings n 