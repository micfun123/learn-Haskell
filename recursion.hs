fac n
  n <= 1 = 1
  otherwuse = n * fac ( n -1 )

sumFromTo :: Int -> Int -> Int
sumFromTo start end =
  case compare start end of
    GT -> 0
    _  -> start + sumFromTo (start + 1) end