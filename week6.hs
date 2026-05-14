import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

alwaysEven :: (Int -> Int) -> [Int] -> Bool
alwaysEven f xs = length (filter even (map f xs)) == length xs

updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
updatePositivesOnly _ [] = []
updatePositivesOnly f (x:xs)
  | x > 0 = f x : updatePositivesOnly f xs
  | otherwise = x : updatePositivesOnly f xs

mult10 :: [Int] -> [Int]
mult10 = map (*10)
 
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

orAll :: [Bool] -> Bool
orAll = foldr (||) False

orAll' :: [Bool] -> Bool
orAll' xs = foldr (\x acc -> x || acc) False xs

sumSquares xs = foldr (\x acc -> x^2 + acc) 0 xs

zeroToTen :: [Int] -> [Int]
zeroToTen xs = filter (<=10) (filter (>=0) xs) 

squareRoots :: [Float] -> [Float]
squareRoots xs = map sqrt (filter (>=0) xs)

countBetween :: Float -> Float -> [Float] -> Int
countBetween low high xs = length (filter (\x -> x >= low && x <= high) xs)

countBetween' :: Float -> Float -> [Float] -> Int
countBetween' low high xs = length (filter isBetween xs)
  where
    isBetween x = x >= low && x <= high 

countBetween''' :: Float -> Float -> [Float] -> Int
countBetween''' low high xs = length (filter (>= low) (filter (<= high) xs))

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f xs = sum (map f xs ) >= 0  

alwaysPositive' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive' f xs = all (\x -> f x >= 0) xs

alwaysPositive''' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive''' f = (>= 0) . sum . map f

productSquareRoots :: [Float] -> Float
productSquareRoots xs = sqrt ( sum ( filter (>0) xs))

productSquareRoots' :: [Float] -> Float
productSquareRoots' = sqrt . sum . filter (>0)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ []     = []
removeFirst p (x : xs)
  | p x = xs
  | otherwise = x : removeFirst p xs


removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f xs = reverse (removeFirst f (reverse xs))
