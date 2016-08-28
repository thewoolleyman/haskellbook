module Ch12Exercises where
import Data.List.Split

notThe :: String -> Maybe String
notThe s = case s of 
  "the" -> Nothing
  _ -> Just s

replaceThe :: String -> String
replaceThe =  unwords . go . words
  where 
    go = map (\x -> if (notThe x) == Nothing then "a" else x)

vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go [] = 0
    go (x:[]) = 0
    go (x:xs) = (go xs) + (
      case notThe x of
        Nothing -> 0
        _ -> if elem (head $ head xs) vowels then 1 else 0
      )

countVowels :: String -> Integer
countVowels = foldr (\x acc-> acc + (if elem x vowels then 1 else 0) ) 0

newtype Word' = Word' String deriving (Eq, Show)


mkWord :: String -> Maybe Word'
mkWord input
  | numConsonants < numVowels = Nothing
  | otherwise = Just $ Word' input
  where numConsonants = (toInteger $ length input) - (countVowels input)
        numVowels = countVowels input


data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n = case n of
                  Zero -> 0
                  (Succ n') -> (natToInteger n') + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ go n
  where
    go 0 = Zero
    go n = Succ(go (n-1))

