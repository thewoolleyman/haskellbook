module Ch16Exercises where

-- 1. a
-- 2. c
-- 3. c
-- 4. c

import Data.Char
import Data.List.Split

shift :: Int -> Char -> Char
shift key ch
  | elem ch ['a'..'z'] = chr $ (mod ((ord ch) - base + key) modBase) + base
  | otherwise = ch
  where
    base = ord 'a'
    modBase = 26

caesar :: Int -> [Char] -> [Char]
caesar = map . shift

unCaesar :: Int -> [Char] -> [Char]
unCaesar = caesar . negate
--- use this to build up things:
-- foldr (++) "" $ take N $ repeat "ally" where N is the length of the phrase to encrypt. That will guarnatee that i can then zip them up
-- zip phrase a where a is the above
-- map caesar with the ord of the letter

buildKeyValuePairs :: [Char] -> [Char] -> [(Char, Int)]
buildKeyValuePairs [] _ = []
buildKeyValuePairs phrase key = reverse $ buildShifters $ (go phrase keys [])
  where
    go [] k ret = ret
    go (x:xs) (x':xs') ret = 
      case x of 
        ' ' -> go xs (x':xs') ((x, ' ') : ret)
        _ -> go xs xs' ((x, x') : ret)
    keys = foldr (++) "" $ take (length phrase) $ repeat key
    buildShifters = map (\(x, y) -> case x of
                                    ' ' -> (x, 0)
                                    _ -> (x, (flip (-) 97 . ord . toLower $ y))
                       )

vigenere :: [Char] -> [Char] -> [Char]
vigenere phrase key  = map (\(x, y) -> flip shift x y) $ buildKeyValuePairs phrase key

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:xs) (x':xs') 
  | x == x' = isSubsequenceOf xs xs'
  | otherwise = isSubsequenceOf a xs'

capitalizeWords :: String -> [(String, String)]
capitalizeWords words = map go $ splitOn " " words
  where go a@(x:xs) = (a, ((toUpper x) : xs ))
