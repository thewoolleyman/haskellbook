module ReaderT where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT ma) =
    ReaderT $ (fmap . fmap)  f ma

instance Applicative m => Applicative (ReaderT r m) where
  pure  = ReaderT . pure . pure
  ReaderT fma <*> ReaderT rm = ReaderT $ (<*>) <$> fma <*> rm

instance Monad m => Monad (ReaderT r m) where
  return = pure

  (ReaderT rma) >>= f =
    ReaderT $
      \r -> rma r >>=
        \a -> runReaderT (f a) r
