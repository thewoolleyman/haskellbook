module MyEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr (\e acc -> case e of
                            Left a -> a : acc
                            Right _ -> acc
               ) []

rights' :: [Either a b] -> [b]
rights' = foldr (\e acc -> case e of
                            Left _ -> acc
                            Right b -> b : acc
                ) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts, rights)
                              where rights = rights' eithers
                                    lefts = lefts' eithers

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f eitherVal =
  case eitherVal of
    Left _ -> Nothing
    Right b -> Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 f2 eitherVal =
  case eitherVal of
    Left a -> f1 a
    Right b -> f2 b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

