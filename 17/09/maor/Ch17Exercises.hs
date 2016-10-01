module Ch17Exercises where
import Data.Monoid
import Control.Applicative

-- 1: []
-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- 2: IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3: (,) a
-- pure :: a -> (b -> (a,b)) a
-- (<*>) :: (b -> (a,b)) -> (,) a -> (a,b)??


-- Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure x = Pair x x
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')


-- Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (Two a f) <*> (Two a' b) = Two (a <> a') (f b)


-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)


-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure a = Three' mempty a a
  (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) (f' b')


-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)


-- Four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  (Four' a0 a1 a2 f) <*> (Four' a0' a1' a2' b) = Four' (a0 <> a0') (a1 <> a1') (a2 <> a2') (f b)


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos s v s' = (,,) <$> s <*> v <*> s'
  