import Data.Char

sumncount :: Integer -> (Integer, Integer)
sumncount x =
  let x1 = abs x
      str = show x1
      numList = [digitToInt x | x <- str]
      n = toInteger $ sum numList
      m = toInteger $ length numList
  in (n, m)
