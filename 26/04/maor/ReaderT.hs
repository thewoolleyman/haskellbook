module ReaderT where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT ma) =
    ReaderT $ (fmap . fmap)  f ma
