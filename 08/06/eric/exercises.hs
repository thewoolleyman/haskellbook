module Chapter8 where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

recSum :: (Eq a, Num a) => a -> a
recSum 1 = 1
recSum n = n + recSum (n - 1)

recMult :: (Integral a) => a -> a -> a
recMult 1 m = m
recMult n m = m + recMult (n - 1) m

data DividedResult =
   Result Integer
  | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom
  | denom == 0 = DividedByZero
  | num < 0 && denom < 0 = Result $ fst $ go (-num) (-denom) 0
  | num < 0 = Result $ negate $ fst $ go (-num) denom 0
  | denom < 0 = Result $ negate $ fst $ go num (-denom) 0
  | otherwise = Result $ fst $ go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 (n + 11)
