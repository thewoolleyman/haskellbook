module Ch10Exer where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True then
    True
  else
    myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\elem accum -> f elem || accum) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = (elem == x) || myElem elem xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem xs = foldr (\x acc -> acc || x == elem) False xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Evaluation Order: [1..10] ([] : (1 : (2 : (3 : (4 : 5)))))
myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

