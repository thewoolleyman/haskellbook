module Main where
import Data.Monoid
import Test.Hspec

data Optional a = 
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (x <> y) 

main :: IO ()
main = hspec $ do
  describe "Monoid Optional" $ do
    it "Only (Sum 1) `mappend` Only (Sum 1) equals Only (Sum {getSum = 2})" $ do
      Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 2})
    it "Only (Product 4) `mappend` Only (Product 2) equals Only (Product {getProduct = 8})" $ do
      Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product {getProduct = 8})
    it "Only (Sum 1) `mappend` Nada equals Only (Sum {getSum = 1})" $ do
      Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum {getSum = 1})
    it "Only [1] `mappend` Nada equals Only [1]" $ do
      Only [1] `mappend` Nada `shouldBe` Only [1]
    it "Nada `mappend` Only (Sum 1) equals Only (Sum {getSum = 1})" $ do
      Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 1})