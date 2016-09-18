module Lib
    ( Trivial (..),
      Identity,
      MyInt,
      Two,
      Three,
      Four,
      BoolConj,
      BoolDisj,
      Or,
      Combine,
      Comp,
      Validation,
      Mem
    ) where

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, mempty, mappend)
import Test.QuickCheck (arbitrary, Arbitrary, oneof, elements)

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where arbitrary = return Trivial


-- Identity a 

newtype Identity a = Identity a deriving (Show)

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') = a == a'

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- MyInt (just experimenting)

newtype MyInt = MyInt Int deriving (Show)
instance Semigroup MyInt where
  (MyInt a) <> (MyInt b) = MyInt (a + b)

instance Monoid MyInt where
  mappend = (<>)
  mempty = MyInt (0)

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

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

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

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

instance Arbitrary BoolConj where 
  arbitrary = do
    a <- elements [False, True]
    return (BoolConj a)

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

instance Arbitrary BoolDisj where 
  arbitrary = do
    a <- elements [False, True]
    return (BoolDisj a)

-- Or a b
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> _ = Fst a
  (Snd a) <> (Fst b) = Fst b
  (Snd a) <> (Snd _) = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a,
           return $ Snd b]

newtype Combine a b = Combine { unCombine :: (a -> b) }

-- HOW DOES THIS WORK????
instance Semigroup b => Semigroup (Combine a b) where
  Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp {unComp=f} <> Comp {unComp=g} = Comp (f . g)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp id

-- VALIDATION

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where 
    Failure' a <> Failure' a' = Failure' (a <> a')
    Failure' a <> Success' b = Failure' a
    Success' b <> Failure' a = Failure' a
    Success' b <> Success' _ = Success' b
        

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Failure' a,
           return $ Success' b]

