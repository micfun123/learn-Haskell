import Text.Printf (printf)


-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type 
data Shape = Circle Float | Rectangle Float Float
  deriving (Show)

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

type Make = String
type Model = String
type HorsePower = Int
type Price = Float

data EngineType = Petrol | Diesel | Electric
  deriving (Show,Eq)
  
data Engine = Engine EngineType HorsePower
  deriving (Show)

data CarName = CarName Make Model
  deriving (Show)

data Car = Car CarName Engine Price
  deriving (Show)

getMake :: Car -> Make
getMake (Car (CarName make _) _ _) = make

getModel :: Car -> Model
getModel (Car (CarName _ model) _ _) = model

getPrice :: Car -> Price
getPrice (Car _ _ price) = price

totalPrice :: [Car] -> Float
totalPrice [] = 0
totalPrice (Car _ _ price : cs) = price + totalPrice cs

testCars :: [Car]
testCars = [Car (CarName "Toyota" "Corolla") (Engine Petrol 132) 20000,
            Car (CarName "Tesla" "Model 3") (Engine Electric 283) 35000,
            Car (CarName "Ford" "F-150") (Engine Diesel 250) 30000,
            Car (CarName "Honda" "Civic") (Engine Petrol 158) 22000 ]

filterByMake :: String -> [Car] -> [Car]
filterByMake manufacturer cs = [c | c <- cs, getMake c == manufacturer]

updatePriceAt :: Int -> Float -> [Car] -> [Car]
updatePriceAt _ _ [] = []
updatePriceAt 0 amount (c : cs) = updatePrice amount c : cs
updatePriceAt index amount (c : cs) = c : updatePriceAt (index - 1) amount cs
updatePrice :: Float -> Car -> Car
updatePrice newPrice (Car name engine _) = Car name engine newPrice

formatCar :: [Car] -> Int -> String
formatCar [] _ = ""
formatCar cars i = printf "%d- %s %s costs %.2f pounds" (i+1) (getMake c) (getModel c)
    (getPrice c)
  where
    c = cars !! i

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
           deriving (Show, Eq, Ord, Enum)

data Season = Spring | Summer | Autumn | Winter
              deriving (Show, Eq)

season' :: Month -> Season
season' m
    | m `elem` [December, January, February] = Winter
    | m `elem` [March, April, May]            = Spring
    | m `elem` [June, July, August]           = Summer
    | otherwise                               = Autumn

season :: Month -> Season
season m = [Spring, Summer, Autumn, Winter, Winter] !! (fromEnum m `div` 3)


numberOfDays :: Month -> Int -> Int
numberOfDays February year
    | year `mod` 4 == 0 = 29
    | otherwise         = 28
numberOfDays month _
    | month `elem` [April, June, September, November] = 30
    | otherwise                                       = 31

data Point = Point Float Float
             deriving (Show)


data PositionedShape = PositionedShape Shape Point
                     deriving (Show)
                    
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) dx dy =
    PositionedShape shape (Point (x + dx) (y + dy))

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ st1 st2) = 1 + numberOfNodes st1 + numberOfNodes st2

isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember n (Node m st1 st2)
    | n == m    = True
    | otherwise = isMember n st1 || isMember n st2

leaves :: Tree -> [Int]
leaves Null = []
leaves (Node n Null Null) = [n]
leaves (Node _ st1 st2) = leaves st1 ++ leaves st2

inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node n st1 st2) = inOrder st1 ++ [n] ++ inOrder st2

preOrder :: Tree -> [Int]
preOrder Null = []
preOrder (Node n st1 st2) = [n] ++ preOrder st1 ++ preOrder st2

postOrder :: Tree -> [Int]
postOrder Null = []
postOrder (Node n st1 st2) = postOrder st1 ++ postOrder st2 ++ [n]

insert :: Int -> Tree -> Tree
insert n Null = Node n Null Null
insert n (Node m st1 st2)
    | n < m     = Node m (insert n st1) st2
    | n > m     = Node m st1 (insert n st2)
    | otherwise = Node m st1 st2  

listToSearchTree :: [Int] -> Tree
listToSearchTree [] = Null
listToSearchTree (n:ns) = insert n (listToSearchTree ns)
