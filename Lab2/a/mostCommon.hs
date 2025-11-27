import Data.List

maxOfString :: [(Char,Int)] -> Int
maxOfString xs = maximum $ map snd xs

mostCommon :: String -> Char
mostCommon xs = 
    let tps = map (\x-> (x,length(filter (==x) xs))) (sort $ nub xs)
        maxCount = maxOfString tps
    in fst $ last $ filter (\(_, x)-> x == maxCount) tps