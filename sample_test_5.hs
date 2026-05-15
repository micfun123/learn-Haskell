-- M21274 MATHFUN Functional Programming - Mega Practice Test
-- 20 Questions

------------------------------------------------------------
-- Exercise 1 - Guards
------------------------------------------------------------
{-
Using guards, write a ticketPrice function.

Age under 12  -> 5
Age under 18  -> 8
Age under 65  -> 12
Otherwise     -> 6
-}

ticketPrice :: Int -> Int
ticketPrice n
    | n < 12 = 5
    | n < 18 = 8
    | n < 65 = 12
    | otherwise = 6
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 2 - Pattern Matching
------------------------------------------------------------
{-
Using pattern matching, write a trafficLight function.

"Red"    -> "Stop"
"Yellow" -> "Wait"
"Green"  -> "Go"
Anything else -> "Unknown"
-}

trafficLight :: String -> String
trafficLight "Red" = "Stop"
trafficLight "Yellow" = "Wait"
trafficLight "Green" = "Go"
trafficLight _ = "Unknown"
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 3 - List Comprehension
------------------------------------------------------------
{-
Using a list comprehension, write a doublePositives function
that doubles only positive integers from a list.
-}

doublePositives :: [Int] -> [Int]
doublePositives xs = [x | x <- xs, x > 0 ]
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 4 - Recursion
------------------------------------------------------------
{-
Using recursion, write a countEven function that counts
how many even numbers are in a list.
-}

countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs)
    | even x = 1 + countEven xs
    | otherwise = countEven xs
    
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 5 - map
------------------------------------------------------------
{-
Using map, write a cubeAll function that cubes every number
in a list.
-}

cubeAll :: [Int] -> [Int]
cubeAll = map (*3)
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 6 - filter
------------------------------------------------------------
{-
Using filter, write a shortStrings function that keeps
only strings with length less than 5.
-}

testStrings :: [String]
testStrings =
    ["cat", "elephant", "sun", "computer", "dog"]

shortStrings :: [String] -> [String]
shortStrings xs = filter (\s -> length s > 5) xs

shortStrings' :: [String] -> [String]
shortStrings' = filter (\s -> length s > 5)

------------------------------------------------------------
-- Exercise 7 - map + filter
------------------------------------------------------------
{-
Using map and filter, write a passingNames function that
takes student-score pairs and returns names of students
with score >= 50.
-}

testStudents :: [(String, Int)]
testStudents =
    [("Alice",72),
     ("Bob",41),
     ("Carol",66),
     ("Dave",30)]

passingNames :: [(String, Int)] -> [String]
passingNames xs = map fst (filter (\(_,s) -> s >= 50) xs)
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 8 - List Comprehension
------------------------------------------------------------
{-
Using a list comprehension, write a pairSums function
that returns sums of all pairs where the first number
is smaller than the second.
-}

pairSums :: [Int] -> [Int]
pairSums xs = [x * y | x <- xs, y <- xs, x < y]
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 9 - Recursion
------------------------------------------------------------
{-
Using recursion, write a largest function that returns
the largest integer in a non-empty list.
-}

largest :: [Int] -> Int
largest [] = 0
largest (x : xs)
    | x > largest xs = x
    | otherwise = largest xs
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 10 - Higher Order Functions
------------------------------------------------------------
{-
Using map, convert a list of temperatures in Celsius
to Fahrenheit.

Formula:
fahrenheit = c * 9 / 5 + 32
-}

toFahrenheit :: [Float] -> [Float]
toFahrenheit = map (\c -> c * 9 / 5 + 32)
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 11 - Tree Recursion
------------------------------------------------------------
{-
Write a treeDepth function that returns the maximum depth
of a binary tree.
-}

data Tree = Leaf | Branch Int Tree Tree
    deriving (Show)

testTree :: Tree
testTree =
    Branch 10
        (Branch 5 Leaf Leaf)
        (Branch 20
            (Branch 15 Leaf Leaf)
            Leaf)

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Branch _ left right) = 1 + max (treeDepth left) (treeDepth right)

------------------------------------------------------------
-- Exercise 12 - List Comprehension
------------------------------------------------------------
{-
Using a list comprehension, write a vowels function that
returns only vowels from a string.
-}

vowels :: String -> String
vowels xs = [c | c <- xs, c `elem` "aeiouAEIOU"]


notVowels :: String -> String
notVowels xs = [c | c <- xs, not (c `elem` "aeiouAEIOU")]


------------------------------------------------------------
-- Exercise 13 - Pattern Matching
------------------------------------------------------------
{-
Using pattern matching, write a firstElement function
that returns the first element of a list.

Return 0 for an empty list.
-}

firstElement :: [Int] -> Int
firstElement [] = 0
firstElement (x:xs) = x



------------------------------------------------------------
-- Exercise 14 - filter with helper function
------------------------------------------------------------
{-
Using filter and a helper function, write a longWords
function that keeps only words with length >= 6.
-}

testWords = ["cat","dog","banana","cake","Portsmouth"]

isLong :: String -> Bool
isLong w = length w >= 6

longWords :: [String] -> [String]
longWords xs = filter isLong xs

------------------------------------------------------------
-- Exercise 15 - map with lambda
------------------------------------------------------------
{-
Using map and a lambda function, add 10 to every number
in a list.
-}

addTen :: [Int] -> [Int]
addTen = map (+ 10) 


------------------------------------------------------------
-- Exercise 16 - Recursive Sum
------------------------------------------------------------
{-
Using recursion, write a multiplyAll function that
multiplies all integers in a list.

Return 1 for an empty list.
-}

multiplyAll :: [Int] -> Int
multiplyAll [] = 1
multiplyAll (x : xs) = x * multiplyAll xs

multiplyAll' :: [Int] -> Int
multiplyAll' x = foldl (*) 1 x
 
------------------------------------------------------------
-- Exercise 17 - List Comprehension
------------------------------------------------------------
{-
Using a list comprehension, write a factors function
that returns all factors of a number.

Example:
factors 12
returns [1,2,3,4,6,12]
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

------------------------------------------------------------
-- Exercise 18 - map + filter
------------------------------------------------------------
{-
Using map and filter, write a squareOdds function that
returns squares of odd numbers only.
-}

squareOdds :: [Int] -> [Int]
squareOdds xs = map (^2) (filter odd xs)

------------------------------------------------------------
-- Exercise 19 - Custom Recursive Structure
------------------------------------------------------------
{-
The Basket type stores item quantities.

Using recursion, write a basketTotal function that
returns the total quantity.
-}

data Basket =
      Empty
    | Item String Int Basket
    deriving (Show)

testBasket :: Basket
testBasket =
    Item "Apple" 4
        (Item "Banana" 2
            (Item "Orange" 5 Empty))

-- basketTotal :: Basket -> Int
-- *** Add your code here ***


------------------------------------------------------------
-- Exercise 20 - Combination Question
------------------------------------------------------------
{-
Using a list comprehension, write a largeSquares function
that returns squares of numbers greater than 5.
-}

largeSquares :: [Int] -> [Int]
largeSquares xs = [x^2 | x <-xs , x > 5]

------------------------------------------------------------
-- TEST FUNCTION
------------------------------------------------------------

test :: IO ()
test = do

    putStrLn "Exercise 1"
    
    putStrLn $ show (ticketPrice 8)
    putStrLn $ show (ticketPrice 16)
    putStrLn $ show (ticketPrice 30)
    putStrLn $ show (ticketPrice 70)
    putStrLn "\nExercise 2"
    putStrLn $ show (trafficLight "Red")
    putStrLn $ show (trafficLight "Green")
    putStrLn $ show (trafficLight "Blue")
    putStrLn "\nExercise 3"
    putStrLn $ show (doublePositives [-2,-1,0,1,2,3])
    putStrLn "\nExercise 4"
    putStrLn $ show (countEven [1,2,3,4,6,7])
    putStrLn "\nExercise 5"
    putStrLn $ show (cubeAll [1,2,3,4])
    putStrLn "\nExercise 6"
    putStrLn $ show (shortStrings testStrings)
    putStrLn "\nExercise 7"
    putStrLn $ show (passingNames testStudents)

    putStrLn "\nExercise 8"
    putStrLn $ show (pairSums [1,2,3,4])

    putStrLn "\nExercise 9"
    putStrLn $ show (largest [5,9,2,11,3])

    putStrLn "\nExercise 10"
    putStrLn $ show (toFahrenheit [0,20,100])
    putStrLn "\nExercise 11"
    putStrLn $ show (treeDepth testTree)
    putStrLn "\nExercise 12"
    putStrLn $ show (vowels "functional")
    putStrLn "\nExercise 13"
    putStrLn $ show (firstElement [7,8,9])
    putStrLn $ show (firstElement [])
    putStrLn "\nExercise 14"
    putStrLn $ show (longWords testWords)
    putStrLn "\nExercise 15"
    putStrLn $ show (addTen [1,2,3])
    putStrLn "\nExercise 16"
    putStrLn $ show (multiplyAll [2,3,4])
    putStrLn "\nExercise 17"
    putStrLn $ show (factors 12)
    putStrLn "\nExercise 18"
    putStrLn $ show (squareOdds [1,2,3,4,5])
    --putStrLn "\nExercise 19"
    --putStrLn $ show (basketTotal testBasket)
    putStrLn "\nExercise 20"
    putStrLn $ show (largeSquares [1,4,6,8])
