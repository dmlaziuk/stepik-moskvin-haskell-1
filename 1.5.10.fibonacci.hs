fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | otherwise = helper n 1 0

helper n prev1 prev2 | n == 0 = prev1
                     | n == 1 = prev1
                     | n == (-1) = prev1
                     | n > 0 = helper  (n - 1) (prev1 + prev2) prev1
                     | n < 0 = helper (n + 1) (prev2 - prev1) prev1
