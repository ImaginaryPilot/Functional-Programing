productEvenOdd :: [Integer] -> (Integer, Integer)
productEvenOdd xs = (product (filter even xs), product (filter odd xs)) 