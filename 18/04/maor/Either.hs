module Either where

-- Hi Eric... 
-- To make this work you'll need to:
-- 1. run `stack install checkers` (and quickcheck if you haven't yet) but in this folder
--    so that it's in a global context
-- 2. run `stack run ghci` in this folder so that it knows about checkers and quickcheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  -- (>>=) = m a -> (a -> m b) -> m b
  (First a) >>= _ = First a
  (Second a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main = do
  let trigger = undefined :: Sum Int (Int, String, Int) 
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger