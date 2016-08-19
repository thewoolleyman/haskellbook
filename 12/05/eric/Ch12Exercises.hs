module Ch12Exercises where

notThe :: String -> Maybe String
notThe str =
  if str == "the" then
    Nothing
  else
    Just str

replaceThe :: String -> String
replaceThe str =
  unwords $ foldr (\word acc -> case notThe word of
          Nothing -> "a" : acc
          Just s -> s : acc) [] (words str)

-- This one was a little tough!
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str =
  let
    checkForVowel count [] = count
    checkForVowel count [_] = count
    checkForVowel count (x:y:xs) =
      if x == "the" && head y `elem` "aeiou" then
        checkForVowel (count + 1) xs
      else
        checkForVowel count xs
  in
    checkForVowel 0 $ words str

countVowels :: String -> Int
countVowels str =
  length $ foldr
    (\s acc -> if s `elem` "aeiou" then acc ++ [s] else acc)
    [] str

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aieou"

mkWord :: String -> Maybe Word'
mkWord str =
  let
    numVowels = length $ foldr (\s acc -> if s `elem` vowels then acc ++ [s] else acc) [] str
    numConsonants = length $ foldr (\s acc -> if s `notElem` vowels then acc ++ [s] else acc) [] str
  in
    if numVowels > numConsonants then
      Nothing
    else
      Just (Word' str)

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger nat =
  case nat of
    Zero -> 0
    Succ n -> 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat num
  | num < 0 = Nothing
  | otherwise =
    Just (buildNat num)
      where buildNat count =
              if count == 0 then
                Zero
              else
                Succ (buildNat (count - 1))

