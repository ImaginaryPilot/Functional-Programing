doubleUs x y = x*2 + y*2
doubleMe x = x + x
doubleSmallNumber x 
    | x <= 100 = x + x
    | otherwise = x
tripleSmallNumber x = if x <= 100 then x + x + x else x
lostNumbers = [4,8,12,16,20]
compareNum x y 
    | x > y = x
    | y > x = y
    | otherwise = 0

multipleOf7 = [x | x <- [1..100], mod x 7 == 0]
oddList = [x | x <- [1..100], mod x 2 /= 0]

rightTriangle = [(a,b,c) | c <- [1..20], b <- [1..c], a<-[1..b], a^2 + b^2 == c^2]

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, elem c ['A'..'Z']]

addFourNum :: Integer -> Integer -> Integer -> Integer -> Integer
addFourNum w x y z = w + x + y + z

qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
    where ys = [y | y <-xs, y <= x]
          zs = [z | z <- xs, z > x]

fac :: Int -> Int
fac 0 = 1
fac n = n*fac(n-1)

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not(x && y)

xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 False False = False
xor1 x y = True

sq :: Integer -> Integer
sq n = n * n
pow4 n = x * x where x = n * n

powInf :: Integer -> Integer -> Integer
powInf x n
    | n == 0 = 1
    | otherwise = x * powInf x (n-1)

evenNum :: Int -> Bool
evenNum n = n == 0 || n > 0 && oddNum (n-1) || n < 0 && oddNum (n+1)

oddNum :: Int -> Bool
oddNum n = n /= 0 && (n > 0 && evenNum (n-1) || n < 0 && evenNum (n+1))

poorFib :: Integer -> Integer
poorFib 0 = 0
poorFib 1 = 1
poorFib n = poorFib (n-1) + poorFib (n-2)

effFib :: Integer -> Integer
effFib n = f n 0 1
    where 
        f 0 a b = a
        f i a b = f (i-1) b (a+b)