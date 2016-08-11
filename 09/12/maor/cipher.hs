module Cipher where

import Data.Char

shift key ch
  | elem ch ['a'..'z'] = chr $ (mod ((ord ch) - base + key) modBase) + base
  | otherwise = ch
  where
    base = ord 'a'
    modBase = 26

caesar = map . shift

unCaesar = caesar . negate

