import Data.Char

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
gradeStudents marks = [(name, grade mark) | (name, mark) <- marks]

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
    