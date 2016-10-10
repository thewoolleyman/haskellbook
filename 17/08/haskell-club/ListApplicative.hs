module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)


take' :: Int -> List a -> List a
take' n Nil = Nil
take' 0 _ = Nil
take' n (Cons a as) = Cons a $ take' (n - 1) as

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend xs Nil = xs
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (Cons f fs) <*> as = fmap f as `mappend` (fs <*> as)
  _ <*> _ = Nil

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  ZipList' (Cons f fs) <*> ZipList' (Cons a as) =
    ZipList' $ Cons (f a) (fs <*> as)
  _ <*> _ = ZipList' Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return (Cons a Nil)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' (Cons a Nil)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main = do
  let trigger = undefined :: List (String, String, String)
  quickBatch $ monoid trigger
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  let trigger2 = undefined :: ZipList' (String, String, String)
  quickBatch $ functor trigger2
  quickBatch $ applicative trigger2

