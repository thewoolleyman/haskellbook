module Jammin where

import Data.List
import Data.Ord

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

-- Cardinality = 4 * 2^64 - product type because of record syntax, Int in Haskell is 64 bits so 2^64
data JamJars = Jam {fruit :: Fruit, jars :: Int}
             deriving (Eq, Show, Ord)

row1 :: JamJars
row1 = Jam {fruit = Plum, jars = 2}

row2 :: JamJars
row2 = Jam {fruit = Apple, jars = 5}

row3 :: JamJars
row3 = Jam {fruit = Plum, jars = 7}

row4 :: JamJars
row4 = Jam {fruit = Blackberry, jars = 3}

row5 :: JamJars
row5 = Jam {fruit = Apple, jars = 10}

row6 :: JamJars
row6 = Jam {fruit = Peach, jars = 4}

allJam :: [JamJars]
allJam = [row1, row2, row3, row4, row5, row6]

-- Function application required because sum(map(..)) thing -> sum (map(..) thing) -> sum([..]) -> Int
totalJars :: [JamJars] -> Int
totalJars = sum . map jars

mostRow :: [JamJars] -> JamJars
mostRow = head . reverse . sortBy (comparing jars)

sortByFruit :: [JamJars] -> [JamJars]
sortByFruit = sortBy (comparing fruit)

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\a b -> fruit a == fruit b) . sortByFruit
