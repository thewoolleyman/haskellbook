{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
module Compose where

import Data.Maybe()
import Control.Applicative
import Control.Monad()



newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- followed approach here: https://www.youtube.com/watch?v=AjtQ0sQaHn0
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure . pure $ x)
  (Compose fn) <*> (Compose x) = Compose $ (pure (<*>) <*> fn) <*> x
