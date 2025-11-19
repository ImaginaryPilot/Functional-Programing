nAnd::Bool->Bool->Bool
nAnd True True = False
nAnd x y = True

nAnd1::Bool->Bool->Bool
nAnd1 x y = not(x && y)

min2::Int->Int->Int
min2 x y = min x y

minThree::Int->Int->Int->Int
minThree x y z = min (min x y) z

charToNum::Char->Int
charToNum c
    | c < '0' = 0
    | c > '9' = 0
    | otherwise = fromEnum c - fromEnum '0'

numberNDroots::Float->Float->Float->Integer
numberNDroots a b c 
    | d < 0 = 0
    | d == 0 = 1
    | otherwise = 2
    where d = b^2 - 4*a*c

numberRoots::Float->Float->Float->Integer
numberRoots a b c
    | a /= 0 = numberNDroots a b c
    | b /= 0 = 1
    | c == 0 = 3
    | otherwise = 0

smallerRoot::Float->Float->Float->Float
smallerRoot a b c
    | numberRoots a b c /= 1 = 0
    | otherwise = (-b - sqrt(b^2 - 4*a*c))/(2*a)

largerRoot::Float->Float->Float->Float
largerRoot a b c
    | numberRoots a b c /= 1 = 0
    | otherwise = (-b + sqrt(b^2 - 4*a*c))/(2*a)

rangeProduct::Integer->Integer->Integer
rangeProduct m n 
    | n < m = 0
    | otherwise = product [m..n]

fac::Integer->Integer
fac n = rangeProduct 1 n

twoToN::Integer->Integer
twoToN n = h n 2
    where
        h 1 a = a
        h n a
            | n == 2*k = h k (a^2)
            | n == 2*k + 1 = h (2*k) (2*a)
            where k = div n 2

maxOccurs::Integer->Integer->(Integer,Integer)
maxOccurs a b = (max a b, if a == b then 2 else 1)

checker::Integer->Integer->Integer->Integer
checker a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) || (a == c) = 2
    | otherwise = 1

maxThreeOccurs::Integer->Integer->Integer->(Integer,Integer)
maxThreeOccurs a b c = (max (max a b) c, checker a b c)

doubleAll::[Integer]->[Integer]
doubleAll list = [2*d | d <- list]

matches::Integer->[Integer]->[Integer]
matches n list = [d | d <-list, d == n]

isElementOf::Integer->[Integer]->Bool
isElementOf n xs
    | matches n xs /= [] = True
    | otherwise = False