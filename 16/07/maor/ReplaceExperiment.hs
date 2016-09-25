module ReplaceExperiment where

  replaceWithP :: b -> Char
  replaceWithP = const 'p'

  lms :: [Maybe [Char]]
  lms = [Just "Ave", Nothing, Just "woohoo"]

  replaceWithP' :: [Maybe [Char]] -> Char
  replaceWithP' = replaceWithP

  liftedReplace :: [Maybe [Char]] -> [Char]
  liftedReplace = fmap replaceWithP

  twiceLifted :: [Maybe [Char]] -> [Maybe Char]
  twiceLifted = (fmap . fmap) replaceWithP

  thriceLifted :: [Maybe [Char]] -> [Maybe [Char]]
  thriceLifted = (fmap . fmap . fmap) replaceWithP

  a = fmap (+1) $ read "[1]" :: [Int]
  b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
  c = (*2) . (\x -> x - 2) $ 1 -- but, but... no fmap?
  d = (return '1' ++)  .  show . (\x -> [x, 1..3]) $ 0

  main :: IO ()
  main = do
    putStr "replaceWithP' lms:  "
    print (replaceWithP' lms)

    putStr "liftedReplace lms:  "
    print (liftedReplace lms)

    putStr "twiceLifted lms:  "
    print (twiceLifted lms) 

    putStr "thriceLifted lms:  "
    print (thriceLifted lms)

    putStr "a ? "
    print (a == [2])

    putStr "b ? "
    print (b == Just ["Hi,lol", "Hellolol"])

    putStr "c ? "
    print (c == (-2))

    putStr "d ? "
    print (d == "1[0,1,2,3]")