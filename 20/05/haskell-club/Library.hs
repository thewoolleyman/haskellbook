module Library where
import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a  = getAny . foldMap (Any . (== a))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum ta | null ta = Nothing
           | otherwise = Just $ foldr1 min ta

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum ta | null ta = Nothing
           | otherwise = Just $ foldr1 max ta

null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (const (All False))

length' :: (Foldable t) => t a -> Int
length' = foldr (const (1 +)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldMap (: [])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x `mappend` acc) mempty
