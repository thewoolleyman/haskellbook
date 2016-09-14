module Lib
    ( Trivial,
      Identity,
      MyInt
    ) where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where arbitrary = return Trivial

newtype Identity a = Identity a deriving (Show)

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') = a == a'

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

newtype MyInt = MyInt Int deriving (Show)
instance Semigroup MyInt where
  (MyInt a) <> (MyInt b) = MyInt (a + b)

instance Eq MyInt where
  (MyInt a) == (MyInt b) = a == b

instance Arbitrary MyInt where 
  arbitrary = do
    a <- arbitrary
    return (MyInt a)


someFunc :: IO ()
someFunc = putStrLn "Hi"
