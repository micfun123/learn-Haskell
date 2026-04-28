import Data.Char

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = print (add 5 3)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

testData :: [Int]
testData = [4,5,7,3,1]

raise :: (String, Int) -> Int -> (String,Int)
raise (b,h) m = (b, h + m)

buildingNames blds = [ b | (b,_) <- blds]


eventNumbers nums = [ x | x <- nums, even x]

head' (x:xs) = x

tail' (_:xs) = xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = sum' xs + x

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = (2*x) : doubleAll xs


nthElm index list = list !! index

-- Lecture 7 - Algebraic Typpes


-- How to define new types

data Degree = Pass | Third | LowerSecond | UpperSecond | First
      deriving (Eq,Ord, Show)

{-
isGood :: Degree -> Bool
isGood UpperSecond = True
isGood First = True
isGood _ = False
-}

isGood :: Degree -> Bool
isGood d = d >= UpperSecond 


data Person = Person String Int
      deriving (Eq, Show)

      
name :: Person -> String
name (Person n _) = n

age :: Person -> Int
age (Person _ n) = n


people =  [Person "Fred" 34, Person "Mark" 21, Person "Sam" 22]

names :: [Person] -> [String]
names p = [n | Person n _ <- p]

names' :: [Person] -> [String]
names' = map name

data Shape = Circle Float | Rectangle Float Float
      deriving (Show,Eq)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perometer (Rectangle h w) = 2 * ( h + w)

data Tree = Null | Node Int Tree Tree
      deriving(Eq,Show)
      
tree0 = Null
tree1 = Node 3 Null Null 
tree2 = Node 5 (Node 3 Null Null) (Node 7(Node 6 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ lnode rnode) = 1 + max (height lnode) ( height rnode)

-- Lecture 8


-- Pure function - Outpute dependens only on the input

square :: Int -> Int
square x = x * x

sayHello :: IO ()
sayHello = do
  putStrLn "hello"
  putStrLn "world"

helloName :: IO()
helloName = do
  putStr "what is your name? "
  name <- getLine
  putStrLn ("Hello " ++ name)

-- getLine :: IO String
-- putStr :: String -> IO()
-- putStrLn :: String -> IO()
-- print :: show a => a -> IO ()
-- print = putStrLn . show

eurosToPounds :: IO ()
eurosToPounds = do
  putStr "Enter a amount"
  eurosS <- getLine
  let euros = read eurosS :: Float
  let pounds = euros / 1.15
  putStrLn ("Pounds is" ++ show pounds)

getFloat :: String -> IO Float
getFloat prompt = do
  putStr prompt
  inputS <- getLine
  let inputF = read inputS :: Float
  return inputF

  -- lecture 4

{-
Strings in HS is the same as C where they are a list of chars
-}

-- tupes

nextTwoNumbers :: Int -> (Int,Int) 
nextTwoNumbers n = (n + 1 , n + 2)


tallest :: (String,Int) -> (String, Int) -> String
tallest (b1, h1) (b2,h2)
  | h1 > h2 = b1
  | h2 > h1 = b2
  | otherwise = "neither"
  
-- getting elements out of a tuple
-- fst TUPLENAME gets the first item
-- snd TUPPLENAME gets the second item
-- Using the _ as we do not care about the val

fst' (x,_) = x
snd' (_,y) = y

raise' :: (String,Int) -> Int -> (String, Int)
raise' (b,n) m = (b,n + m)

-- Strings

-- All the elements in the list must be the same type


-- A list of chars will be converted in to a string
-- ['a', 'b', 'b'] -> "abc"

-- "fred" ++ "dy" -> "freddy"
-- behind the sceenes its joining two lists of chars and then outputting a string


f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:zs) = replicate n x ++ f n zs

sumFromTo :: Int -> Int -> Int
sumFromTo start end =
  case compare start end of
    GT -> 0
    _  -> start + sumFromTo (start + 1) end


square' :: Int -> Int
square' n = n * n

mult2 :: Int -> Int
mult2 x = 2 * x

mult4 :: Int -> Int
mult4 x = mult2 (mult2 x)

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen num = num * 10

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = areaOfCircle ( r ) * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 =  sqrt ((x1 - x2)^2 + (y1 - y2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = (a /= b) && (b /= c) && (a /= c)

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

isEven :: Int -> Bool
isEven a = divisibleBy a 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) / 3

applyDiscount :: Float -> Int -> Float
applyDiscount price percent = price * (1 - fromIntegral percent / 100)

absolute :: Int -> Int
absolute n = if n < 0 then -n else n


absolute' :: Int -> Int
absolute' n
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
1. sumThree 3 5 7 | Definition
   -> 3 + 5 + 7  | Addition
   -> 8 + 7 | Addition
   -> 15

   sumThree 8 (1 + 3) 2 | Definition
   -> 8 + (1 + 3) + 2 | Addition
   -> 8 + 4 + 2 | Addition
   -> 14

2. threeDifferent 1 4 2  | Definition
   -> 1 /= 4 && 4 /= 2 && 1 /= 2   | Comparison
   -> True && True && True  | Logical AND
   -> True

   threeDifferent 1 7 7  | Definition
   -> 1 /= 7 && 7 /= 7 && 1 /= 7  | Comparison
   -> True && False && True   | Logical AND
   -> False

3. howManyEqual 3 5 2  | Definition
   -> 3 == 5 && 5 == 2 (False)  | Comparison
   -> 3 == 5 || 5 == 2 || 3 == 2 (False)  | Comparison
   -> 0 (otherwise)

   howManyEqual 5 2 5  | Definition
   -> 5 == 2 && 2 == 5 (False)  | Comparison
   -> 5 == 2 || 2 == 5 || 5 == 5 (True)  | Comparison
   -> 2
-}

type StudentMark = (String, Int)



sumDifference :: Int -> Int -> (Int,Int)
sumDifference a b = (a + b, a - b)

grade :: (String, Int) -> Char
grade (name, mark)
    | mark < 0 || mark > 100 = error "Invalid mark"
    | mark >= 70            = 'A'
    | mark >= 60            = 'B'
    | mark >= 50            = 'C'
    | mark >= 40            = 'D'
    | otherwise             = 'F'


capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark < 0 || mark > 100 = error "Invalid mark"
    | otherwise             = (name, min mark 40)


firstNumbers :: Int -> [Int]
firstNumbers n
    | n <= 0    = []
    | otherwise = [1..n]

firstSquares :: Int -> [Int]
firstSquares n
    | n <= 0    = []
    | otherwise = [x^2 | x <- firstNumbers n]


capitalise :: String -> String
capitalise str = [toUpper c | c <- str]

onlyDigits :: String -> String
onlyDigits str = [c | c <- str, isDigit c]

capMarks :: [StudentMark] -> [StudentMark]
capMarks marks = [capMark mark | mark <- marks]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents marks = [(name, grade (name, mark)) | (name, mark) <- marks]



duplicate:: String -> Int -> String
duplicate str 0 = ""
duplicate str n = str ++ duplicate str (n-1)


{-

duplicate :: String -> Int -> String
duplicate str n = concat [str | _ <- [1..n]]

-}


divisors :: Int -> [Int]
divisors n
    | n <= 0    = []
    | otherwise = [x | x <- [1..n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = divisors n == [1, n]


split :: [(a,b)] -> ([a],[b])
split xs = ([fst p | p <- xs], [snd p | p <- xs])


{-
split:: [(a,b)] -> ([a],[b])
split [] = ([], [])
split ((x,y):xs) = (x:xs1, y:xs2)
    where (xs1, xs2) = split xs
-}
    

{-:
countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
| x == ' ' = 1 + countSpaces xs
| otherwise = countSpaces xs


mergelists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
| x <= y = x : mergeLists xs (y:ys)
| otherwise = y : mergeLists (x:xs) ys

-}

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:_) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:zs) = x : x : zs


rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate [x, y] = [x, y]
rotate (x:y:zs) = y : x : zs

listLength :: [a] -> Int
listLength [] = 0
listLength (x:zs) = 1 + listLength zs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:zs) = x * multAll zs


andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:zs) = x && andAll zs

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:zs) = x || orAll zs

countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers n (x:zs)
    | n == x = 1 + countIntegers n zs
    | otherwise = countIntegers n zs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:zs)
    | n == x = removeAll n zs
    | otherwise = x : removeAll n zs


removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst n (x:zs)
    | n == x = x : removeAll n zs
    | otherwise = x : removeAllButFirst n zs



    
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((n, m):xs)
    | name == n = m : listMarks name xs
    | otherwise = listMarks name xs

sorted :: [Int] -> Bool
sorted (x:xs) = case xs of
    [] -> True
    (y:ys) -> x <= y && sorted xs


prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence (x:xs) (y:ys)
    | x == y = subSequence xs ys
    | otherwise = subSequence (x:xs) ys

everOtherItem :: [Int] -> [Int]
everOtherItem lst = [x | (x,y) <- zip lst [0..], mod y 2 == 1 ]

rev [] = []
rev (x:xs) = rev xs ++ [x]


isodd n = mod n 2 == 1

test1321321 arr = sum $ filter isodd arr


len :: [a] -> Int
len lst = (last [y | (x,y) <- zip lst [0..]]) + 1
