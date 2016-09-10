module MonoidQuickChecks where
  import Control.Monad
  import Data.Monoid
  import Test.QuickCheck

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidLeftIdentity m = (mempty <> m) == m

  monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidRightIdentity m = (m <> mempty) == m

  data Bull = 
      Fools
    | Twoo
    deriving (Eq, Show)

  instance Arbitrary Bull where
    arbitrary = 
      frequency [(1, return Fools), (1, return Twoo)]


  instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

  type BullMappend = Bull -> Bull -> Bull -> Bool  

  main :: IO ()
  main = do
    quickCheck (monoidAssoc :: String -> String -> String -> Bool)
    quickCheck (monoidLeftIdentity :: String -> Bool)
    quickCheck (monoidRightIdentity :: String -> Bool)
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)