module PhhhbbtttEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Prelude hiding (Left, Right)

data PhhhbbtttEither b a = 
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a) = Left (f a)
  fmap _ (Right b) = Right b

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure = Left
  Left f <*> Left a = Left (f a)
  Right b <*> Right b' = Right (b `mappend` b')
  Right b <*> _ = Right b
  _ <*> Right b = Right b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: PhhhbbtttEither String (String, String, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  -- quickBatch $ monad trigger