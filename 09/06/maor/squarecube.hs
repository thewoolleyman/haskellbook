module SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

tuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

numTupes = length tuples


-- alternative:

allLessThan :: Integer -> [Integer] -> [Integer]
allLessThan x xs = takeWhile (< x) xs

allLessThan50 :: [Integer] -> [Integer]
allLessThan50 xs = allLessThan 50 xs

tuples' :: [(Integer, Integer)]
tuples' = [(x, y) | x <- allLessThan50 mySqr, y <- allLessThan50 myCube] 

