module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a' = Identity (f a')

instance Monad Identity where
  return = pure
  Identity a >>= f = f a


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: Identity (String, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger