seqA :: Integer -> Integer
seqA n = let 
  helper n prev1 prev2 prev3 | n == 0 = prev1
                             | n == 1 = prev2
                             | n == 2 = prev3
                             | otherwise = helper (n - 1) prev2 prev3 (prev3 + prev2 - 2 * prev1)
  in helper n 1 2 3 
