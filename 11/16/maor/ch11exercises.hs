module Ch11Exercises where

-- 1. a
-- 2. c
-- 3. c
-- 4. c

import Data.Char

shift key ch
  | elem ch ['a'..'z'] = chr $ (mod ((ord ch) - base + key) modBase) + base
  | otherwise = ch
  where
    base = ord 'a'
    modBase = 26


caesar = map . shift


unCaesar = caesar . negate
--- use this to build up things:
-- foldr (++) "" $ take N $ repeat "ally" where N is the length of the phrase to encrypt. That will guarnatee that i can then zip them up
-- zip phrase a where a is the above
-- map caesar with the ord of the letter


