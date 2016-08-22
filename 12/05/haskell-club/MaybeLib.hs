module MaybeLib where

import Data.Maybe (fromMaybe)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- mayybee 0 (+1) Nothing = 0
-- mayybee 0 (+1) (Just 1) = 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def f maybeVal =
  case maybeVal of
    Just n -> f n
    Nothing -> def

mayybee' :: b -> (a -> b) -> Maybe a -> b
mayybee' def f maybeVal = fromMaybe def $ f <$> maybeVal

fromMaybe' :: a -> Maybe a -> a
fromMaybe' def = mayybee' def id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

-- Just 1 -> [1]
-- Nothing -> []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just n) = [n]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\m acc -> maybeToList m ++ acc) []

-- [Just 1, Just 2, Just 3] -> Just [1,2,3]
-- [Just 1, Nothing, Just 2] -> Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes
  | length m == length maybes = Just m
  | otherwise = Nothing
  where m = catMaybes maybes

