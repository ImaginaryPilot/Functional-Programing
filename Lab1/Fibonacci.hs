fib::Integer->Integer
fib n = f n 1 2
    where
        f 0 a b = a
        f i a b = f (i-1) b (a+b)

factor::Integer->Integer
factor n 
    | n <= fib n
    | otherwise = 

numberOfFiboSums::Integer->Integer
numberOfFiboSums n = 