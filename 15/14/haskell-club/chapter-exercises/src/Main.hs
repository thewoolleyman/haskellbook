module Main where

import Test.QuickCheck
import Data.Semigroup

------------------------------------------------------
-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


------------------------------------------------------
-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

------------------------------------------------------
-- Combine a b
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine { unCombine = f } <> Combine { unCombine = g } = Combine (f <> g)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc Trivial)

