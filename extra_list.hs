-- Basic: squares of 1..10
squares :: [Int]
squares = [x^2 | x <- [1..10]]
-- [1,4,9,16,25,36,49,64,81,100]

-- With a guard: even squares only
evenSquares :: [Int]
evenSquares = [x^2 | x <- [1..10], even x]
-- [4,16,36,64,100]

-- Cartesian product
pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1..3], y <- [1..3], x /= y]
-- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]

-- String processing: uppercase only
uppers :: String -> String
uppers str = [c | c <- str, c `elem` ['A'..'Z']]
-- uppers "Hello World" => "HW"

-- Pythagorean triples up to n
pyTriples :: Int -> [(Int, Int, Int)]
pyTriples n = [(a, b, c) | c <- [1..n],
                            b <- [1..c],
                            a <- [1..b],
                            a^2 + b^2 == c^2]
-- pyTriples 20 => [(3,4,5),(6,8,10),(5,12,13),(8,15,17),(9,12,15)]

-- FizzBuzz with list comp
fizzBuzz :: [String]
fizzBuzz = [fb x | x <- [1..20]]
  where fb x
          | x `mod` 15 == 0 = "FizzBuzz"
          | x `mod` 3  == 0 = "Fizz"
          | x `mod` 5  == 0 = "Buzz"
          | otherwise        = show x

-- Matrix transpose (list of lists)
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([] : _) = []
transpose' xss = [head xs | xs <- xss] : transpose' [tail xs | xs <- xss]

-- Nested comprehension: multiplication table
mulTable :: [[Int]]
mulTable = [[x * y | x <- [1..5]] | y <- [1..5]]

greaterthan :: [(Int, Int)] -> [Bool]
greaterthan pairs = [x > y | (x, y) <- pairs]

