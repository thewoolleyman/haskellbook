module Ch23Exercises where

newtype Moi s a = Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap f (Moi s) = _undefined
