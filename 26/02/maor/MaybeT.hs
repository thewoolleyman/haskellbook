module MaybeT where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
