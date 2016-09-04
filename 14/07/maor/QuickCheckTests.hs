module QuickCheckTests where

import Test.QuickCheck
import Data.List (sort)

half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Bool
halfIdentity x = ((*2) . half $ x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Double -> Double -> Bool
plusCommutative x y = x + y == y + x

plusAssociativeMul :: Int -> Int -> Int -> Bool
plusAssociativeMul x y z = x * (y * z) == (x * y) * z

plusCommutativeMul x y = x * y == y * x



main :: IO ()
main = do
  quickCheck halfIdentity
  quickCheck (listOrdered :: [Int] -> Bool)
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck plusAssociativeMul
  quickCheck (plusCommutativeMul :: Int -> Int -> Bool)
