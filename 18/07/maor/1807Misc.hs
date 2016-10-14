module Misc where

import Control.Applicative
import Test.Hspec

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

main :: IO ()
main = hspec $ do
  describe "j" $ do
    it "passes all the book tests" $ do
      j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
      j (Just (Just 1)) `shouldBe` Just 1
      j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
      j (Nothing) `shouldBe` (Nothing :: Maybe Int)
