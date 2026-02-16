-- lecture 4

{-
Strings in HS is the same as C where they are a list of chars
-}

-- tupes

nextTwoNumbers :: Int -> (Int,Int) 
nextTwoNumbers n = (n + 1 , n + 2)


tallest :: (String,Int) -> (String, Int) -> String
tallest (b1, h1) (b2,h2)
  | h1 > h2 = b1
  | h2 > h1 = b2
  | otherwise = "neither"
  
-- getting elements out of a tuple
-- fst TUPLENAME gets the first item
-- snd TUPPLENAME gets the second item
-- Using the _ as we do not care about the val

fst' (x,_) = x
snd' (_,y) = y

raise :: (String,Int) -> Int -> (String, Int)
raise (b,n) m = (b,n + m)

-- Strings
-- All the elements in the list must be the same type


-- A list of chars will be converted in to a string
-- ['a', 'b', 'b'] -> "abc"

-- "fred" ++ "dy" -> "freddy"
-- behind the sceenes its joining two lists of chars and then outputting a string

