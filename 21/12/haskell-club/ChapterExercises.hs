module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)
  sequenceA (Identity a) = fmap Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data Optional a =
    Nada
  | Yep a

instance Traversable Optional where
  sequenceA Nada = mempty
  sequenceA (Yep a) = fmap Yep a

instance Functor Optional where
  fmap f (Yep a) = Yep $ f a
  fmap _ Nada = Nada

instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ Nada = mempty

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  (Yep f) <*> (Yep a) = Yep (f a)

main :: IO ()
main = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger)

