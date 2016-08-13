module Folds where

-- 1 -> c
-- 3 -> c
-- 4 -> a

fivea :: String
fivea = foldr (++) [] ["woot", "WOOT", "woot"] 

fiveb :: String
fiveb = foldr max [] ["fear", "is", "the", "little", "death"]

fivec :: Bool
fivec = foldr (&&) True [False, True]

fived :: Bool
fived = foldr (||) False [False, True]

fivee :: String
fivee = foldr ((++) . show) "" [1..5]

fivef :: String
fivef = foldr (const . show) "a" [1..5]

fiveg :: String
fiveg = foldr const "0" ["tacos"]

fiveh :: String
fiveh = foldl (flip const) "0" ["burritoes"]

fivei :: String
fivei = foldl (flip (const . show)) "z" [1..5]
