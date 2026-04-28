f :: [Int] -> [Int]
f lst = [x| (x,y) <- zip lst [0..], mod y 2 == 0 ]

