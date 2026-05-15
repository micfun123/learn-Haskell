-- M21274 MATHFUN Functional Programming - Larger Practice Test

{-
Exercise 1 - 3 marks
--------------------
Using guards, write a movieRating function that takes an integer score
(from 0 to 100) and returns:

85 or above -> "Excellent"
70 or above -> "Good"
50 or above -> "Average"
otherwise    -> "Poor"
-}

-- movieRating :: Int -> String


{-
Exercise 2 - 4 marks
--------------------
Using a list comprehension and the classifyAge function below,
write a classifyPeople function that takes a list of
person-age pairs and returns a list of person-category pairs.

Running your function on testPeople should give:

[("Alice","Adult"),("Ben","Teen"),
 ("Charlie","Child"),("Diana","Senior")]
-}

testPeople :: [(String, Int)]
testPeople =
    [("Alice", 25),
     ("Ben", 15),
     ("Charlie", 8),
     ("Diana", 70)]

classifyAge :: Int -> String
classifyAge age
    | age >= 65 = "Senior"
    | age >= 18 = "Adult"
    | age >= 13 = "Teen"
    | otherwise = "Child"

-- classifyPeople :: [(String, Int)] -> [(String, String)]
-- *** Add your code here ***


{-
Exercise 3 - 4 marks
--------------------
Using recursion, write a longWords function that takes a minimum length
and a list of strings and returns only the strings whose length is
greater than or equal to the minimum length.
-}

testWords :: [String]
testWords =
    ["cat", "elephant", "tree", "encyclopedia", "sun"]

-- longWords :: Int -> [String] -> [String]
-- *** Add your code here ***


{-
Exercise 4 - 4 marks
--------------------
Using higher-order functions (map and filter), write a passingScores
function that takes a pass mark and a list of student-score pairs
and returns a list of names of students who passed.
-}

testScores :: [(String, Int)]
testScores =
    [("Alice", 72),
     ("Bob", 40),
     ("Carol", 55),
     ("Dave", 91),
     ("Eve", 33)]

-- passingScores :: Int -> [(String, Int)] -> [String]
-- *** Add your code here ***


{-
Exercise 5 - 4 marks
--------------------
Using higher-order functions, write a squareEvens function that takes
a list of integers and returns a list containing the squares of only
the even numbers.

Example:
squareEvens [1,2,3,4,5]
returns [4,16]
-}

-- squareEvens :: [Int] -> [Int]
-- *** Add your code here ***


{-
Exercise 6 - 3 marks
--------------------
The Tree type below represents a binary tree of integers.

Write a countLeaves function that returns the number of Leaf values
in a Tree.
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

-- countLeaves :: Tree -> Int
-- *** Add your code here ***


{-
Exercise 7 - 4 marks
--------------------
Using a list comprehension, write a pairProducts function that takes
a list of integers and returns all products of pairs where the first
number is smaller than the second.

Example:
pairProducts [1,2,3]

returns:
[2,3,6]

because:
1*2 = 2
1*3 = 3
2*3 = 6
-}

-- pairProducts :: [Int] -> [Int]
-- *** Add your code here ***


{-
Exercise 8 - 4 marks
--------------------
The Basket type below stores item names and quantities.

Using recursion, write a totalItems function that returns the total
quantity of all items in the Basket.
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

-- totalItems :: Basket -> Int
-- *** Add your code here ***


{-
Test function
--------------
Uncomment the tests for the exercises you attempt.
-}

test :: IO ()
test = do

    putStrLn "Exercise 1"

    putStrLn $ "95 -> " ++ show (movieRating 95)
    putStrLn $ "74 -> " ++ show (movieRating 74)
    putStrLn $ "51 -> " ++ show (movieRating 51)
    putStrLn $ "22 -> " ++ show (movieRating 22)

    -- putStrLn "\nExercise 2"

    -- putStrLn $ show (classifyPeople testPeople)

    -- putStrLn "\nExercise 3"

    -- putStrLn $ show (longWords 5 testWords)
    -- putStrLn $ show (longWords 8 testWords)

    -- putStrLn "\nExercise 4"

    -- putStrLn $ show (passingScores 50 testScores)
    -- putStrLn $ show (passingScores 70 testScores)

    -- putStrLn "\nExercise 5"

    -- putStrLn $ show (squareEvens [1,2,3,4,5,6])

    -- putStrLn "\nExercise 6"

    -- putStrLn $ show (countLeaves testTree)
    -- putStrLn $ show (countLeaves Leaf)

    -- putStrLn "\nExercise 7"

    -- putStrLn $ show (pairProducts [1,2,3])
    -- putStrLn $ show (pairProducts [2,4,6])

    -- putStrLn "\nExercise 8"

    -- putStrLn $ show (totalItems testBasket)
    -- putStrLn $ show (totalItems Empty)