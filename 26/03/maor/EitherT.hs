module EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  (EitherT fab) <*> (EitherT mea) =
    EitherT $ (<*>) <$> fab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT mea >>= f =
    EitherT $ mea >>= \ea ->
      case ea of
        Left e -> return $ Left e
        Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema
