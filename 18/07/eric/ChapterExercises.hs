module ChapterExercises where

import Prelude hiding (Left, Right)

data Nope a =
  NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

data PhhhbbtttEither a b =
    Left a
  | Right b

instance Functor (PhhhbbtttEither a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

instance Applicative (PhhhbbtttEither a) where
  pure = Right
  (Left a) <*> _ = Left a
  _ <*> (Left a) = Left a
  (Right f) <*> (Right a) = Right (f a)

instance Monad (PhhhbbtttEither a) where
  return = pure
  (>>=) = undefined

