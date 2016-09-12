module Main where

import Test.QuickCheck

half x = x / 2

halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = x == 2 * half x

prop_wrong_half :: Double -> Bool
prop_wrong_half x | x == 0 = x == half x
                  | x > 0 = half x < x
                  | otherwise = half x > x

prop_half_identity :: Double -> Bool
prop_half_identity x = x == halfIdentity x

main :: IO ()
main = do
  quickCheck prop_half
  quickCheck prop_wrong_half
  quickCheck prop_half_identity
