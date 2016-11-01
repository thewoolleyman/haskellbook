module Library where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = foldr (\x acc -> x == a || acc) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = undefined

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = undefined

-- how would I do this using foldr or foldMap?
null :: (Foldable t) => t a -> Bool
null = (0 ==) . length

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> True) False

length' :: (Foldable t) => t a -> Int
length' = foldr (const (1 +)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x `mappend` acc) mempty
