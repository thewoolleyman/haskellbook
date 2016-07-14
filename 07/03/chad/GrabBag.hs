module GrabBag where

-- ex1

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

-- Last one is different:
-- *GrabBag> :t mTh1
-- mTh1 :: Num a => a -> a -> a -> a
-- *GrabBag> :t mTh2
-- mTh2 :: Num a => a -> a -> a -> a
-- *GrabBag> :t mTh3
-- mTh3 :: Num a => a -> a -> a -> a
-- *GrabBag> :t mTh4
-- mTh4 :: Integer -> Integer -> Integer -> Integer

-- ex2 - answer: a
ex2 = mTh4 3

-- *GrabBag> :t ex2
-- ex2 :: Integer -> Integer -> Integer

-- ex3a

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

addOneIfOddAnon n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

-- ex3b

addFive x y = (if x > y then y else x) + 5
addFiveAnon = \x -> \y -> (if x > y then y else x) + 5

-- ex3c

mflipAnon f = \x -> \y -> f y x
mflip f x y = f y x