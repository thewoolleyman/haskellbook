module Currying where

import Data.Char

diffFunc :: String -> Int -> Char -> String
diffFunc string int char = string ++ " " ++ [intToDigit int] ++ " " ++ [char]

