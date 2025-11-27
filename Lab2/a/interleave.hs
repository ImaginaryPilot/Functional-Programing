import Data.List

interleaveRR :: [String] -> String 
interleaveRR xss = concat (transpose xss)