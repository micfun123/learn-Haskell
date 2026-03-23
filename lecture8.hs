
-- Lecture 8


-- Pure function - Outpute dependens only on the input

square :: Int -> Int
square x = x * x

main :: IO ()
main = putStrLn "words"

sayHello :: IO ()
sayHello = do
  putStrLn "hello"
  putStrLn "world"

helloName :: IO()
helloName = do
  putStr "what is your name? "
  name <- getLine
  putStrLn ("Hello " ++ name)
