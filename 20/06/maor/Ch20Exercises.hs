module Ch20Exercises where
import Data.Monoid ((<>))

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c
