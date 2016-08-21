{-# LANGUAGE FlexibleInstances #-}
module LogiCGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
  tooMany (n, n') = (n + n') > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany a && tooMany b
