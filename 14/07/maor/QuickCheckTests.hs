module QuickCheckTests where

import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

gen :: (Arbitrary a) => Gen a
gen = do
  a <- arbitrary
  return a

divisor :: Gen Float
divisor = arbitrary `suchThat` (/= 0)

half x = x / 2
halfIdentity = ((*2) . half)

prop_half :: Property
prop_half =
  forAll divisor
  (\x  -> (half x) * 2 == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered =
  forAll (genList :: Gen [Int])
  (\x -> listOrdered (sort x) == True)

genList :: (Arbitrary a, Eq a) => Gen [a]
genList = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return [a,b,c]

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z


plusCommutative :: Double -> Double -> Bool
plusCommutative x y = x + y == y + x

plusAssociativeMul :: Int -> Int -> Int -> Bool
plusAssociativeMul x y z = x * (y * z) == (x * y) * z

plusCommutativeMul x y = x * y == y * x

--using the identity wrapper
data Identity a = Identity a deriving (Eq, Ord, Show)
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

quotremTest x (Positive y) = (quot x y) * y + (rem x y) == x
divmodTest x (Positive y) = (div x y) * y + (mod x y) == x

expoAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z
expoCommutative x y = x ^ y == y ^ x

prop_reverseListTwice :: Property
prop_reverseListTwice = 
  forAll (genList :: Gen [Int] )
  (\x -> (reverse . reverse) x  == id x)

capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs


twice f = f . f
fourTimes = twice . twice

prop_capitalizeWord = 
  forAll (gen :: Gen [Char])
  (\x -> (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x  == fourTimes capitalizeWord x))

prop_sort = 
  forAll (gen :: Gen[Int])
  (\x -> (sort x == twice sort x) && (sort x == fourTimes sort x))

data Fool = 
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = do
  oneof [return $ Fulse,
         return $ Frue]

genFool' :: Gen Fool
genFool' = do
  frequency [(3, return $ Fulse),
             (1, return $ Frue)]

main :: IO ()
main = do
  quickCheck prop_half
  quickCheck prop_listOrdered
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck plusAssociativeMul
  quickCheck (plusCommutativeMul :: Int -> Int -> Bool)
  quickCheck (divmodTest :: Int -> (Positive Int) -> Bool)
  quickCheck (quotremTest :: Int -> (Positive Int) -> Bool)
  quickCheck (expoAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (expoCommutative :: Int -> Int -> Bool)
  quickCheck (prop_reverseListTwice)
  quickCheck (prop_capitalizeWord)
  quickCheck (prop_sort)
