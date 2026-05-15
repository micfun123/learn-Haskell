-- M21274 MATHFUN Functional Programming in-class test

{-
Exercise 1 - 4 marks
--------------------
Using guards or pattern matching, write a letterGrade function that takes an
integer score and returns a letter grade as a String, where 70 or above is "A",
60 or above is "B", 50 or above is "C", 40 or above is "D", and anything
below 40 is "F".
-}
letterGrade :: Int -> String
letterGrade n
  | n >= 70 = "A"
  | n >= 60 = "B"
  | n >= 50 = "C"
  | n >= 40 = "D"
  | otherwise = "F"
-- *** Uncomment the line above and add your code here ***

{-
Exercise 2 - 4 marks
--------------------
Using a list comprehension and the classifySpeed function below, write a
classifyVehicleSpeeds function that takes a list of vehicle-speed pairs
(like the testVehicles list), and returns a list of vehicle-description pairs.
Running your function on testVehicles should give:
  [("Car","Fast"),("Bicycle","Slow"),("Jet","Supersonic"),("Bus","Moderate")]
-}
testVehicles :: [(String, Int)]
testVehicles = [("Car", 120), ("Bicycle", 25), ("Jet", 900), ("Bus", 80)]

classifySpeed :: Int -> String
classifySpeed speed
    | speed >= 500 = "Supersonic"
    | speed >= 100 = "Fast"
    | speed >= 50  = "Moderate"
    | otherwise    = "Slow"

classifyVehicleSpeeds :: [(String, Int)] -> [(String, String)]
classifyVehicleSpeeds x = [(ver,speedlev) | (ver,temp) <- x, let speedlev = classifySpeed temp]
-- *** Uncomment the line above and add your code here ***

{-
Exercise 3 - 4 marks
--------------------
Using recursion, write a passingStudents function that takes a pass mark and a
list of name-score pairs (like the testStudents list) and returns a list of
names of students whose score is greater than or equal to the pass mark.
-}
testStudents :: [(String, Int)]
testStudents = [("Alice", 72), ("Bob", 45), ("Carol", 58), ("Dave", 91), ("Eve", 38)]

passingStudents :: Int -> [(String, Int)] -> [String]
passingStudents _ [] = []
passingStudents minScor ((name, score):otherstus)
  | score >= minScor = name : passingStudents minScor otherstus
  | otherwise = passingStudents minScor otherstus 


-- *** Uncomment the line above and add your code here ***

{-
Exercise 4 - 3 marks
--------------------
The Tree type definition below represents a binary tree of integers, and
testTree is an example Tree. Write a treeSum function that takes a Tree and
returns the sum of all integer values stored in it.
-}
data Tree = Leaf | Branch Int Tree Tree
    deriving (Show)

testTree :: Tree
testTree = Branch 5 (Branch 3 Leaf Leaf) (Branch 8 (Branch 6 Leaf Leaf) Leaf)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Branch n left right ) = n + treeSum left + treeSum right


-- *** Uncomment the line above and add your code here ***

{-
Test function. Use this function to test your solutions. You should uncomment
all lines relating to the exercises you have attempted, but should not change
anything else.
To test the functionality of your code we will only run this function so
make sure that:
  * you uncommented all appropriate lines in the function
  * you comment out any incomplete/non-working solutions
-}
test :: IO ()
test = do
    putStrLn "Exercise 1"
    putStrLn $ "  85 - " ++ show (letterGrade 85)
    putStrLn $ "  63 - " ++ show (letterGrade 63)
    putStrLn $ "  51 - " ++ show (letterGrade 51)
    putStrLn $ "  42 - " ++ show (letterGrade 42)
    putStrLn $ "  30 - " ++ show (letterGrade 30)
    putStrLn "Exercise 2"
    putStrLn $ "  " ++ show (classifyVehicleSpeeds testVehicles)
    putStrLn "Exercise 3"
    putStrLn $ "  Pass mark 50 - " ++ show (passingStudents 50 testStudents)
    putStrLn $ "  Pass mark 60 - " ++ show (passingStudents 60 testStudents)
    putStrLn "Exercise 4"
    putStrLn $ "  Sum of testTree    - " ++ show (treeSum testTree)
    putStrLn $ "  Sum of a Leaf      - " ++ show (treeSum Leaf)
    putStrLn $ "  Sum of single node - " ++ show (treeSum (Branch 7 Leaf Leaf))
