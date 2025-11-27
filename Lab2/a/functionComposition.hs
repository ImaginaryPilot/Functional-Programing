compose :: [a -> a] -> a -> a
compose (x:xs) = foldl (.) x xs