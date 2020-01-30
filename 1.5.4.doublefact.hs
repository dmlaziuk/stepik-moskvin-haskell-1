doubleFact :: Integer -> Integer
doubleFact n | n == 0 = 1
             | n == 1 = 1
             | otherwise = n * doubleFact (n-2)
