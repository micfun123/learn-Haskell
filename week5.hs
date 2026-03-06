
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


