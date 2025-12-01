-- Infinite list of decimal digits of pi: 3,1,4,1,5,9,...
piDigits :: [Integer]
piDigits = digits 1 0 1 1 3 3
  where
    digits q r t k n l
      | 4*q + r - t < n*t =
          -- We can safely emit digit n
          n : digits (10*q) (10*(r - n*t)) t k n' l
      | otherwise =
          -- Need to refine the approximation first
          digits (q*k) ((2*q + r)*l) (t*l) (k+1) n'' (l+2)
      where
        n'  = (10*(3*q + r)) `div` t - 10*n
        n'' = (q*(7*k + 2) + r*l) `div` (t*l)

neededPiDigits :: Int -> Int
neededPiDigits n = h piDigits [0..fromIntegral n] [] 0
    where
        h (d:ds) digitsLeft window len
            | null digitsLeft = len - 1
            | otherwise = 
                let 
                    window' = takeLast 4 (window)++[d]
                    toRemove = buildNumber window'
                    digitsLeft' = filter (`notElem` toRemove) digitsLeft
                in h ds digitsLeft' window' (len+1)
        
        buildNumber (a:b:c:d:_) = [d, c*10 + d, b*100 + c*10 + d, a*1000 + b*100 + c*10 + d] 
        buildNumber (a:b:c:_) = [c, b*10 + c, a*100 + b*10 + c] 
        buildNumber (a:b:_) = [b, a*10 + b]
        buildNumber (a:_) = [a]
        buildNumber _ = [] 

        takeLast n xs = drop (length xs - n) xs