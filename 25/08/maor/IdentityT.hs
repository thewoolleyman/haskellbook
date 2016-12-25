{-# LANGUAGE InstanceSigs #-}

module IdentityT where
import Control.Monad

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative f) => Applicative (IdentityT f) where
  pure = pure
  IdentityT fab <*> IdentityT fa = IdentityT $ fab <*> fa

instance (Monad f) => Monad (IdentityT f) where
  return = pure
  --(>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= \a -> runIdentityT $ f a
