module Lib
    ( Trivial,
      Identity,
      MyInt,
      Two,
      Three,
      Four,
      BoolConj,
      BoolDisj
    ) where

import Data.Semigroup
import Test.QuickCheck

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where arbitrary = return Trivial


-- Identity a 

newtype Identity a = Identity a deriving (Show)

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') = a == a'

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- MyInt (just experimenting)

newtype MyInt = MyInt Int deriving (Show)
instance Semigroup MyInt where
  (MyInt a) <> (MyInt b) = MyInt (a + b)

instance Eq MyInt where
  (MyInt a) == (MyInt b) = a == b

instance Arbitrary MyInt where 
  arbitrary = do
    a <- arbitrary
    return (MyInt a)

-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- Three a b c

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- Four a b c d

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)


-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where 
  arbitrary = do
    a <- elements [False, True]
    return (BoolConj a)

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where 
  arbitrary = do
    a <- elements [False, True]
    return (BoolDisj a)

