module Garden where

type Gardener = String

data GardenNF = 
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

