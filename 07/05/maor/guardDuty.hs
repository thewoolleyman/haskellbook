module GuardDuty where

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135   = "too low"
  | x > 145   = "too high"
  | otherwise = "just right"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
  | y >= 0.9    = 'A'
  | y >= 0.8    = 'B'
  | y >= 0.7    = 'C'
  | y >= 0.59   = 'D'
  | otherwise   = 'F'
  where y = x / 100

numbers x 
  | x < 0     = -1
  | x == 0    = 0
  | x > 0     = 1
