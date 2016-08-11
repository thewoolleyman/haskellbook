module Cipher where

import Data.Char

shift num ch
  | elem ch ['a'..'z'] = chr $ (mod ((ord ch) - 97 + num) 26) + 97
  | otherwise = ch

caesar = map . shift

unCaesar = caesar . negate

