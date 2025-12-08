primes::[Integer]
primes = 2:[]

sieve::[Integer]->[[Integer]]
sieve i = [i + j + 2*i*j | j <- [i..]] : sieve (i+1)
    
merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
    | x == y = merge xs ys
    | x < y = x:merge xs (y:ys)
    | x > y = merge (x:xs) ys