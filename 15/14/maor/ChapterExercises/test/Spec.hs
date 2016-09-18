module Main where

import Lib
import Test.QuickCheck
import qualified Data.Monoid as M
import qualified Data.Semigroup as S

semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool 
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

monoidLeftIdentity :: (Eq m, M.Monoid m) => m -> Bool
monoidLeftIdentity a = (a M.<> mempty) == a

monoidRightIdentity :: (Eq m, M.Monoid m) => m -> Bool
monoidRightIdentity a = (mempty M.<> a) == a


main :: IO ()
main = do 
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (semigroupAssoc :: (Identity Trivial) -> (Identity Trivial) -> (Identity Trivial) -> Bool)
  quickCheck (semigroupAssoc :: (Identity MyInt) -> (Identity MyInt) -> (Identity MyInt) -> Bool)
  quickCheck (semigroupAssoc :: (Two Trivial MyInt) -> (Two Trivial MyInt) -> (Two Trivial MyInt) -> Bool)
  quickCheck (semigroupAssoc :: (Three Trivial MyInt MyInt) -> (Three Trivial MyInt MyInt) -> (Three Trivial MyInt MyInt) -> Bool)
  quickCheck (semigroupAssoc :: (Four Trivial MyInt MyInt Trivial) -> (Four Trivial MyInt MyInt Trivial) -> (Four Trivial MyInt MyInt Trivial) -> Bool)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: (Or Int Bool) -> (Or Int Bool) -> (Or Int Bool) -> Bool)
  quickCheck (semigroupAssoc :: (Validation MyInt Bool) -> (Validation MyInt Bool) -> (Validation MyInt Bool) -> Bool)


  -- Monoid
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: (Identity Trivial) -> Bool)
  quickCheck (monoidRightIdentity :: (Identity Trivial) -> Bool)
  quickCheck (monoidLeftIdentity :: (Two Trivial MyInt) -> Bool)
  quickCheck (monoidRightIdentity :: (Two Trivial MyInt) -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)