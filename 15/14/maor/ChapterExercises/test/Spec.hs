module Main where

import Semigroup'
import Test.QuickCheck
import Data.Semigroup


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



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