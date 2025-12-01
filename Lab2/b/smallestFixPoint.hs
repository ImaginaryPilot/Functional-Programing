fixpoint :: (Integer -> Integer) -> Integer
fixpoint f = head [n | n <- [0..], f n == n]