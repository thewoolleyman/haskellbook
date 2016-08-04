module EnumFromTo where

enumFromTo' :: (Enum a, Eq a) => a -> a -> [a]
enumFromTo' from to = go from to [] 
  where go from to li
          | from == to = li ++ [to]
          | otherwise = go (succ from) to (li ++ [from])


