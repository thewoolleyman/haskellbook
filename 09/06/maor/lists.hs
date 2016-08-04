module Lists where

myWords li = go li []
  where 
        dropPart f = drop 1 . dropWhile f  
        takePart f x = [(takeWhile f x)]
        go input result
          | input == [] = result
          | otherwise = go (dropPart (/= ' ') input) (result ++ (takePart (/= ' ') input))



