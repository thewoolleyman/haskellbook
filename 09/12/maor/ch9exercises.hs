module Ch9Exercises where 
import Data.Char

upperOnly :: [Char] -> [Char]
upperOnly text = filter isUpper text

capFirst :: [Char] -> [Char]
capFirst (x:xs) = toUpper x : xs
capFirst [] = []

capAll :: [Char] -> [Char]
capAll [] = []
capAll (x:xs) = toUpper x : capAll xs

capHead :: [Char] -> Char
capHead = toUpper . head
