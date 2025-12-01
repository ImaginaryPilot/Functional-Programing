random :: [Integer]
random = h 2773639 25214903917 11 (2^45)
    where 
        h x a c m = x : h (mod (a*x + c) m) a c m