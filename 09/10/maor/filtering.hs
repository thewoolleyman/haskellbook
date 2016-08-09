module Filtering where

q1 :: Integral a => [a] -> [a]
q1 li = filter (\x -> mod x 3 == 0) li

q2 :: Integral a => [a] -> Int
q2 = length . q1

q3 :: String -> [[Char]]
q3 = filter (\x -> not (elem x ["the", "a"] )) . words
