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
  foldMap f (Two _ b) = f b

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldMap f (Three _ _ c) = f c

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = f b <> f c

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b c d) = f b <> f c <> f d
