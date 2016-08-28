module MyEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr(\x acc -> case x of
                          (Left a) -> a : acc
                          _ -> acc) []

rights' :: [Either a b] -> [b]
rights' = foldr(\x acc -> case x of
                          (Right b) -> b : acc
                          _ -> acc) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' li = ((lefts' li), (rights' li))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' ::  (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left a) = Nothing
eitherMaybe'' f (Right b) = Just $ either' id f (Right b)

