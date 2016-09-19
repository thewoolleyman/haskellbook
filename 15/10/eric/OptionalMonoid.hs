module OptionalMonoid where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada n = n
  mappend n Nada = n
  mappend (Only x) (Only y) = Only (x <> y)

