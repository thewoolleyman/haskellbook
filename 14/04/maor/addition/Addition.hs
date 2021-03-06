module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multiplyBy :: Integral a => a -> a -> a
multiplyBy num multi = go num multi 0
  where go n m count
          | m == 0 = count
          | otherwise = go n (m -1) (count + n)

trivialInt :: Gen Int
trivialInt = return 1

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

-- usage requires specifying types: sample ( genTuple :: Gen (Int, Float))
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- usage reuires specifying types: sample ( genThreeple :: Gen (Int, Float, Char))
genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
--    it "1 + 1 is greater than 1" $ do
--      (1 + 1) > 1 `shouldBe` True
--    it "2 + 2 is equal to 4" $ do
--      2 + 2 `shouldBe` 4
    it "15 divided by 4 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4,2)
    it "5 multiplied by 3 is 15" $ do
      multiplyBy 5 3 `shouldBe` 15
    it "5 multiplied by 0 is 0" $ do
      multiplyBy 5 0 `shouldBe` 0
    it "5 multplied by 1 is 5" $ do
      multiplyBy 5 1 `shouldBe` 5
    it "x + 1 is always greater than x" $ do
      property $ \x-> x + 1> (x :: Int)

