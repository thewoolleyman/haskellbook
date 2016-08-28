module MyMaybes where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def _ Nothing = def
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe def a = mayybee def id a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
-- this is loco
catMaybes li = [x | Just x <- li]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe li 
  | (length li) == (length res) = Just res
  | otherwise = Nothing
  where res = catMaybes li
