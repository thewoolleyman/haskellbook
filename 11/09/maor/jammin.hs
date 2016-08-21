module Jammin where
import Data.List

data Fruit = 
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars = Jam {
    fruit :: Fruit,
    quantity :: Int
  } deriving (Eq, Show, Ord)

-- cardinality: Card(Int) * Card(Fruit) = 18446744073709551616 * 4
--    = 73786976294838206464

row1 = Jam Peach 5
row2 = Jam Plum 10
row3 = Jam Apple 6
row4 = Jam Blackberry 100
row5 = Jam Plum 100
row6 = Jam Apple 20
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars = foldr (\x acc -> acc + (quantity x)) 0

mostRow :: [JamJars] -> JamJars
mostRow = foldr1
    (\x acc -> if (quantity x) > (quantity acc) then x else acc)

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sameFruit :: JamJars -> JamJars -> Bool
sameFruit (Jam k _) (Jam k' _) = 
  case compare k k' of
    EQ -> True
    _ -> False

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy sameFruit . sortJam
