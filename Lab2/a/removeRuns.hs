removeRuns::String->String
removeRuns xs = map fst (filter (\(x,y)-> x /= y) (zip xs (tail xs))) ++ [last xs]