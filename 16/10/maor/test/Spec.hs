module Main where

import Lib
import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x) == (fmap (g . f) x))
  


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    a2 <- arbitrary
    b <- arbitrary
    return (Four' a0 a1 a2 b)

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
  
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: (Identity Int))
  
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: (Pair Int))
  
  quickCheck $ \x -> functorIdentity (x :: Two Int String)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: (Two Int Double))

  quickCheck $ \x -> functorIdentity (x :: Three Int String Double)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: (Three Int String Double))

  quickCheck $ \x -> functorIdentity (x :: Three' Int String)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: (Three' Int Double))

  quickCheck $ \x -> functorIdentity (x :: Four' Int String)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: (Four' Int Double))



