module Ch10Exercises where

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStop stops vowels = filter (\(x,y,z) -> x == 'p') $ [(x,y,z) | x <- stops, y <- vowels, z <- stops ]

nounVerbNoun :: [[Char]] -> [[Char]] -> [([Char], [Char], [Char])]
nounVerbNoun nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr
        (\x acc -> x || acc) False

myOr'' :: [Bool] -> Bool
myOr'' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr
          (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = (elem == x) || myElem elem xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem = foldr
          (\x acc -> x == elem || acc) False

