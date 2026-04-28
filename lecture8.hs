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

-- getLine :: IO String
-- putStr :: String -> IO()
-- putStrLn :: String -> IO()
-- print :: show a => a -> IO ()
-- print = putStrLn . show

eurosToPounds :: IO ()
eurosToPounds = do
  putStr "Enter a amount"
  eurosS <- getLine
  let euros = read eurosS :: Float
  let pounds = euros / 1.15
  putStrLn ("Pounds is" ++ show pounds)

getFloat :: String -> IO Float
getFloat prompt = do
  putStr prompt
  inputS <- getLine
  let inputF = read inputS :: Float
  return inputF
