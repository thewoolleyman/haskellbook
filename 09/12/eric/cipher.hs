module Cipher where

import Data.Char (toUpper, chr, ord, isAlphaNum)

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (first:rest) =
  toUpper first : rest

allToUpper :: String -> String
allToUpper [] = []
allToUpper (s:str) = toUpper s : allToUpper str

allToUpper' :: String -> String
allToUpper' str =
  [toUpper x | x <- str]

shiftChar :: Int -> Char -> Char
shiftChar shift =
  chr . (+) shift . ord

caesarShift :: Int -> Char -> Char
caesarShift shift char 
  | isAlphaNum char = chr $ (ord char - base + shift) `mod` 26 + base
  | otherwise = char
    where
      base :: Int
      base = ord 'a'

caesarCipher :: Int -> String -> String
caesarCipher shift =
  map (caesarShift shift)

-- America's Cipher - (POINT) FREEDOM
-- "Make America point free again" - Jeff Schomay 2016
jeffCaesarCipher :: Int -> String -> String
jeffCaesarCipher = map . caesarShift

{-jeffCaesarCipher =
  \shift -> (\string -> map (caesarShift shift) string)-}

-- "Let's dispel this fiction that Jeff Schomay doesn't know what he's
-- doing. Jeff Schomay knows exactly what he's doing. He's undertaking
-- a systematic effort to make every Haskell function we write point free."
unJeffCaesarCipher :: Int -> String -> String
unJeffCaesarCipher = jeffCaesarCipher . negate

unCaesarCipher :: Int -> String -> String
unCaesarCipher shift =
  map (caesarShift $ negate shift)


