module Lists where

myWords li = go li []
  where go input result
          | input == [] = result
          | otherwise = go (drop 1 (dropWhile (/= ' ') input)) (result ++ [(takeWhile (/= ' ') input)]) 
