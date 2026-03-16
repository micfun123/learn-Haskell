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
