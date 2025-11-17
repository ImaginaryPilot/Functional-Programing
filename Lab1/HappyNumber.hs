sqDigits::Integer->[Integer]
sqDigits 0 = []
sqDigits n = sqDigits (div n 10) ++ [(mod n 10)^2]

sumSqDigits::[Integer]->Integer
sumSqDigits n = sum n

isHappy::Integer->Bool
isHappy n = h n []
    where
        h 1 _ = True
        h x a
            | elem x a = False
            | otherwise =  h (sumSqDigits(sqDigits(x))) (x:a)

countHappyNumbers::Integer->Integer->Int
countHappyNumbers a b = length [d | d<-[a..b], isHappy d] 