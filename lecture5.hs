import Data.Char

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
