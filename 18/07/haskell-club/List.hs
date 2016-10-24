module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend xs Nil = xs
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as

instance Applicative List where
  pure = flip Cons Nil
  (Cons f fs) <*> as = fmap f as `mappend` (fs <*> as)
  _ <*> _ = Nil

instance Monad List where
  return = pure
-- (>>=) :: Monad m => m    a -> (a -> m    b) -> m    b
--                     List a -> (a -> List b) -> List b
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = f a `mappend` (>>=) as f

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return (Cons a Nil)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: List (String, String, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

