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

