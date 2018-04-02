module Ch4Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

f :: (a, b) -> (c, d) -> ((b,d), (a,c))
f one two = ((snd one, snd two), (fst one, fst two))

