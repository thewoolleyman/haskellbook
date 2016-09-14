module Main where

import Lib
import Test.QuickCheck
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



main :: IO ()
main = do 
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (semigroupAssoc :: (Identity Trivial) -> (Identity Trivial) -> (Identity Trivial) -> Bool)
  quickCheck (semigroupAssoc :: (Identity MyInt) -> (Identity MyInt) -> (Identity MyInt) -> Bool)
  