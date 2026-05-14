import Data.Char 
import Data.List

-- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Fibonacci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Sum of a list
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- Reverse a list
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

-- Map (reimplemented)
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

-- Flatten nested lists
flatten :: [[a]] -> [a]
flatten []       = []
flatten (xs:xss) = xs ++ flatten xss

-- Power (exponentiation)
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

-- Count elements matching a predicate
countIf :: (a -> Bool) -> [a] -> Int
countIf _ []     = 0
countIf p (x:xs)
  | p x       = 1 + countIf p xs
  | otherwise = countIf p xs

countChars :: Char -> String -> Int
countChars _ []     = 0
countChars c (x:xs)
  | c == x    = 1 + countChars c xs
  | otherwise = countChars c xs

isPalindrome :: String -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome (x:xs)
  | x == last xs = isPalindrome (init xs)
  | otherwise    = False

replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new str@(x:xs)
  | take (length old) str == old = new ++ replace old new (drop (length old) str)
  | otherwise                    = x : replace old new xs

wordCount :: String -> Int
wordCount [] = 0
wordCount str =
  let (_, rest) = break (== ' ') (dropWhile (== ' ') str)
  in if null (dropWhile (== ' ') str)
     then 0
     else 1 + wordCount rest