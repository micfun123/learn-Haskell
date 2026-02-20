
absolute :: Int -> Int
absolute n
  | n < 0     = -n
  | otherwise = n


sign :: Int -> Int
sign n
  | n < 0     = -1
  | n == 0    = 0
  | otherwise = 1


howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
  | a == b && b == c           = 3
  | a == b || b == c || a == c = 2
  | otherwise                  = 0


sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = diagonal a + diagonal b + diagonal c
  where
    diagonal side = sqrt 2 * side


taxiFare :: Int -> Float
taxiFare distance = 2.20 + additional
  where
    additional
      | distance <= 10 = fromIntegral distance * 0.50
      | otherwise      = (10 * 0.50) + (fromIntegral (distance - 10) * 0.30)


howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = count (fromIntegral a) + count (fromIntegral b) + count (fromIntegral c)
  where
    avg = fromIntegral (a + b + c) / 3
    count x = if x > avg then 1 else 0

validDate :: Int -> Int -> Bool
validDate day month
  | month < 1 || month > 12 = False
  | day < 1                 = False
  | month == 2              = day <= 28
  | elem month [4, 6, 9, 11] = day <= 30
  | otherwise               = day <= 31


daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2               = if isLeap then 29 else 28
  | elem month [4, 6, 9, 11] = 30
  | otherwise                = 31
  where
    isLeap = mod year 4 == 0

{-
WRITTEN EXERCISES

1. sumThree 3 5 7
   -> 3 + 5 + 7
   -> 8 + 7
   -> 15

   sumThree 8 (1 + 3) 2
   -> 8 + (1 + 3) + 2
   -> 8 + 4 + 2
   -> 14

2. threeDifferent 1 4 2
   -> 1 /= 4 && 4 /= 2 && 1 /= 2
   -> True && True && True
   -> True

   threeDifferent 1 7 7
   -> 1 /= 7 && 7 /= 7 && 1 /= 7
   -> True && False && True
   -> False

3. howManyEqual 3 5 2
   -> 3 == 5 && 5 == 2 (False)
   -> 3 == 5 || 5 == 2 || 3 == 2 (False)
   -> 0 (otherwise)

   howManyEqual 5 2 5
   -> 5 == 2 && 2 == 5 (False)
   -> 5 == 2 || 2 == 5 || 5 == 5 (True)
   -> 2
-}