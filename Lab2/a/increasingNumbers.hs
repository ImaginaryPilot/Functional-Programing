increasing :: [Integer] -> [Integer]
increasing xs = map snd (filter (\(x,y) -> x < y) (zip xs (tail xs)))