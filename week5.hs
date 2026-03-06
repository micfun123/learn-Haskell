
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


type StudentMark = (String, Int)
testData :: [StudentMark]
testData =[ 
    ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)]

    
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((n, m):xs)
    | name == n = m : listMarks name xs
    | otherwise = listMarks name xs

