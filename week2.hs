absolute :: Int -> Int
absolute n
  | n < 0 = -n
  | otherwise = n

sign :: Int -> Int
sign n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
  | a == b && b == c = 3
  | a == b || b == c || a == c = 2
  | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = d1 + d2 + d3
  where
    diagonal side = sqrt 2 * side
    d1 = diagonal a
    d2 = diagonal b
    d3 = diagonal c

taxiFare :: Int -> Float
taxiFare distance = baseFare + totalFare
  where
    baseFare = 2.20
    totalFare
      | distance <= 10 = fromIntegral distance * 0.50
      | otherwise = 10 * 0.50 + fromIntegral (distance - 10) * 0.30

validDate :: Int -> Int -> Bool
validDate day month
  | month < 1 || month > 12 = False
  | day < 1 = False
  | elem month [1, 3, 5, 7, 8, 10, 12] && day > 31 = False
  | elem month [4, 6, 9, 11] && day > 30 = False
  | month == 2 && day > 29 = False
  | otherwise = True

{-
Evaluation and calculation exercises:

1. sumThree 3 5 7
   ⇝ 3 + 5 + 7
   ⇝ 8 + 7
   ⇝ 15

   sumThree 8 (1 + 3) 2
   ⇝ 8 + (1 + 3) + 2
   ⇝ 8 + 4 + 2
   ⇝ 12 + 2
   ⇝ 14

2. threeDifferent 1 4 2
   ⇝ 1 /= 4 && 4 /= 2 && 1 /= 2
   ⇝ True && True && True
   ⇝ True

   threeDifferent 1 7 7
   ⇝ 1 /= 7 && 7 /= 7 && 1 /= 7
   ⇝ True && False && True
   ⇝ False

3. howManyEqual 3 5 2
   ⇝ 3 == 5 && 5 == 2  -- False
   ⇝ 3 == 5 || 5 == 2 || 3 == 2 -- False
   ⇝ 0

   howManyEqual 5 2 5
   ⇝ 5 == 2 && 2 == 5 -- False
   ⇝ 5 == 2 || 2 == 5 || 5 == 5 -- True
   ⇝ 2
-}