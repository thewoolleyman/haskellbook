module MaybeT where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-- wow, I still dont understand this...
instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  MaybeT ma >>= f =
    MaybeT $ ma >>= \a ->
      case a of
        Nothing -> return Nothing
        Just a -> runMaybeT . f $ a
