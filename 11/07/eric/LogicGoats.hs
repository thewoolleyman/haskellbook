{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (n, m) = (n + m) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany n || tooMany m

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
