integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + sums 0 1)
  where 
    h = (b - a) / 1000
    sums acc n | n > 999 = acc
               | otherwise = sums (acc + f (a + n * h)) (n + 1)
