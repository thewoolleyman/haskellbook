module MaybeMonoid where
  import Data.Monoid
  import Test.QuickCheck

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidLeftIdentity m = (mempty <> m) == m

  monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidRightIdentity m = (m <> mempty) == m

  data Optional a = 
    Nada
    | Only a
    deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada x = x
    mappend x Nada = x
    mappend (Only x) (Only y) = Only (x <> y) 
  
  newtype First' a =
    First' {getFirst' :: Optional a }
    deriving (Eq, Show)

  instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) (First' (Only a)) = First' (Only a)
    mappend (First' (Only a)) _ = First' (Only a)
    mappend (First' Nada) (First' Nada) = First' Nada

  instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
      a <- arbitrary
      frequency [(1, return (First' Nada)), (1, return (First' (Only a)))]


  firstMappend :: First' a -> First' a -> First' a
  firstMappend = mappend

  type FirstMappend =
       First' String
    -> First' String
    -> First' String
    -> Bool

  main :: IO ()
  main = do    
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: First' String -> Bool) 
    quickCheck (monoidRightIdentity :: First' String -> Bool)