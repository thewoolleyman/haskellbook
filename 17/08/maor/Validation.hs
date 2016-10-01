module Validation where

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure fa <*> Failure fa' = Failure (fa `mappend` fa')
  Failure fa <*> Success sa = Failure fa
  Success sa <*> Failure fa = Failure fa
  Success sa <*> Success sa' = Success (sa sa')
