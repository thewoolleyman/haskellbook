module Ch10Exercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

threeTuples :: String -> String -> [(Char, Char, Char)]
threeTuples cons vows = zip3 cons vows cons

filterThreeTuples :: String -> String -> [(Char, Char, Char)]
filterThreeTuples cons vows = filter (\(x, _, _) -> x == 'p' ) $ threeTuples cons vows

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\cand accum -> f cand || accum) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (i:is) = e == i || myElem e is

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = foldr (\i acc -> e == i || acc) False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldr (\x acc -> acc ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) =
  if f x then
    x : myFilter f xs
  else
    myFilter f xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr (\x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy ordFunc (x:y:xs) =
  case ordFunc x y of
    GT -> x
    _ -> myMaximumBy ordFunc (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy ordFunc (x:y:xs) =
  case ordFunc x y of
    LT -> x
    _ -> myMinimumBy ordFunc (y:xs)

