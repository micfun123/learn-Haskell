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
