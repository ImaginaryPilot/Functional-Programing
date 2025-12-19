primes::[Integer]
primes = 2:map(\c->2*c+1) sieve

nums::Integer->[Integer]
nums i = [i + j + 2*i*j | j <- [i..]]

sieve::[Integer]
sieve = merge [1..] [nums i | i <-[1..]]

merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
    | x == y = merge xs ys
    | x < y = x:merge xs (y:ys)
    | x > y = merge (x:xs) ys