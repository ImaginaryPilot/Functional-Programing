divisors:: Integer->[Integer]
divisors n = 2:[3,5..n]

factors::Integer->Integer
factors n
    | even n = factors (n/2)
    
    [d | d <- [2 .. n]]

totient::Integer->Integer
totient n = 