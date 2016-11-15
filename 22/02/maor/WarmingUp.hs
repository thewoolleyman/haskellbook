module WarmingUp where

import Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = fmap rev cap

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = rev >>= \r -> cap >>= \c -> return (c, r)
