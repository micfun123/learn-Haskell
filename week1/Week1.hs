
timesTen :: Int -> Int
timesTen num = num * 10

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = areaOfCircle ( r ) * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 =  sqrt ((x1 - x2)^2 + (y1 - y2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = (a /= b) && (b /= c) && (a /= c)

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

isEven :: Int -> Bool
isEven a = divisibleBy a 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) / 3

applyDiscount :: Float -> Int -> Float
applyDiscount price percent = price * (1 - fromIntegral percent / 100)

absolute :: Int -> Int
absolute n = if n < 0 then -n else n

