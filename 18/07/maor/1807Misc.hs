module Misc where

import Control.Monad
import Control.Applicative ((<*>))
import Test.Hspec
import Data.Maybe

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)


main :: IO ()
main = hspec $ do
  describe "j" $ do
    it "passes all the book tests" $ do
      j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
      j (Just (Just 1)) `shouldBe` Just 1
      j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
      j (Nothing) `shouldBe` (Nothing :: Maybe Int)
  describe "l1" $ do
    it "passes some basic tests" $ do
      l1 (+ 1) [1, 2, 3] `shouldBe` [2, 3, 4]
      l1 (+ 1) (Just 1) `shouldBe` Just 2
      l1 (+ 1) Nothing `shouldBe` Nothing
  describe "l2" $ do
    it "passes some basic tests" $ do
      l2 (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5, 6, 7, 6, 7, 8, 7, 8, 9]
      l2 (+) (Just 1) (Just 2) `shouldBe` Just 3
      l2 (+) Nothing (Just 2) `shouldBe` Nothing
  describe "a" $ do
    it "passes some basic tests" $ do
      a [1,2,3] [(+ 1),(+ 5)] `shouldBe` [2, 3, 4, 6, 7, 8]