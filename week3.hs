import Prelude hiding ((||), (&&), gcd)
infixr 3 &&

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True  _ = False

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

{-
(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

-}

{-
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False
-}

(&&) :: Bool -> Bool -> Bool
True  && p = p
False && _ = False


{-
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False
-}

{-
(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True
-}

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || p = p

exOr :: Bool -> Bool -> Bool
exOr True p = not p
exOr False p = p

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True  x _ = x
ifThenElse False _ y = y

daysInMonth :: Int -> Int
daysInMonth 1  = 31 
daysInMonth 2  = 28 
daysInMonth 3  = 31 
daysInMonth 4  = 30 
daysInMonth 5  = 31 
daysInMonth 6  = 30 
daysInMonth 7  = 31 
daysInMonth 8  = 31 
daysInMonth 9  = 30 
daysInMonth 10 = 31 
daysInMonth 11 = 30
daysInMonth 12 = 31 

validDate :: Int -> Int -> Bool
validDate day month = day >= 1 && day <= daysInMonth month


sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares ( n-1 )

{-
sumSquares :: Int -> Int
sumSquares n
  | n == 0    = 0
  | otherwise = (n * n) + sumSquares (n - 1)
-}

power :: Int -> Int -> Int
power _ 0 = 1
power n p = n * power n (p - 1)

{-
power :: Int -> Int -> Int
power _ 0 = 1
power n p = n * power n (p - 1)
-}

sumFromTo :: Int -> Int -> Int
sumFromTo start end
  | start > end = 0
  | otherwise   = start + sumFromTo (start + 1) end

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (mod a b)

{-
gcd :: Int -> Int -> Int
gcd a b
  | a == b    = a
  | a > b     = gcd (a - b) b
  | otherwise = gcd a (b - a) 
-}


intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

{-

findRoot :: Int -> Int -> Int
findRoot n s
  | s * s <= n = s
  | otherwise  = findRoot n (s - 1)
-}

findRoot :: Int -> Int -> Int
findRoot n s = check (s * s <= n)
  where
    check True  = s
    check False = findRoot n (s - 1)
