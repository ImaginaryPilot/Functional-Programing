balancedParens :: String -> Bool
balancedParens xs = 
    let sums = scanl (+) 0 $ map (\x -> if x == '(' then 1 else -1 ) $ filter (\x -> x == '(' || x == ')') xs
    in all (>=0) sums && last sums == 0
