module MyMaybe where

isJust :: Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    Just _ -> True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee i f val =
  case val of
    Just n -> f n
    Nothing -> i

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal = mayybee defaultVal id

listToMaybe :: [a] -> Maybe a
listToMaybe xs =
  case xs of
    [] -> Nothing
    (x:_) -> Just x

maybeToList :: Maybe a -> [a]
maybeToList val =
  case val of
    Nothing -> []
    Just n -> [n]

catMaybes :: [Maybe a] -> [a]
catMaybes =
  foldr (\m acc -> case m of
    Just n -> n : acc
    Nothing -> acc) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybeList =
  if length maybeList /= length onlyJusts then
    Nothing
  else
    Just onlyJusts
  where onlyJusts = catMaybes maybeList

