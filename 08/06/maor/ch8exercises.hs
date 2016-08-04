module Ch8Exercises where

{- Multiple Choice:
 - 1 -> d
 - 2 -> b
 - 3 -> d
 - 4 -> b
 -}

cattyConny :: String -> String -> String
cattyConny x y = x ++  " mrow " ++ y

flippy:: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

{- Currying Review
 - 1 -> String
 - 2 -> String
 - 3 -> String
 - 4 -> String
 - 5 -> String
 - 6 -> String
 -}

 {- recursion
  - dividedBy 15 2
  - go 15 2 0
  - go 13 2 1
  - go 11 2 2
  - go 9  2 3
  - go 7  2 4
  - go 5  2 5
  - go 3  2 6
  - go 1  2 7
  -}

-- really I'd want a to be Ord as well so I can handle the negative number case 
-- instead of bottoming out
sumNums :: (Eq a, Num a) => a -> a
sumNums x 
  | x == 0 = 0
  | otherwise = x + sumNums(x - 1)

multiplyNums :: (Integral a) => a -> a -> a
  --
multiplyNums x y = go 1
  where go  count 
          | count == y = x
          | otherwise = x + go(count + 1)

data DividedResult = 
    Result Integer
  | DividedByZero 
  deriving (Show)

dividedBy :: Integral a => a -> a-> DividedResult
dividedBy num denom 
  | denom == 0 = DividedByZero
  | otherwise = signCheck (go (abs num) (abs denom) 0)
                where go n d count
                        | n < d = count
                        | otherwise = go (n - d) d (count + 1)
                      signCheck result
                        | num > 0 && denom < 0 = Result (negate result)
                        | num < 0 && denom > 0 = Result (negate result)
                        | otherwise = Result result

mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11 
