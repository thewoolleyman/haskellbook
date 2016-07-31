module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool from to =
  [from, to]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
  | from == to = [from]
  | otherwise = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from == to = [from]
  | otherwise = from : eftInt (succ from) to

eftChar :: Char -> Char -> String
eftChar from to
  | from == to = [from]
  | otherwise = from : eftChar (succ from) to

