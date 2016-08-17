module Cardinality where

-- Cardinality = 1
data PugType = PugData

-- Cardinality = 3
data Airline =
      PapuAir
    | CatapultsR'US
    | TakeYourChancesUnited

-- Int16 cardinality is 2^16 so 65536

-- maxBound::Int = 9223372036854775807 and minBound::Int = -9223372036854775808 - Huge cardinality
-- maxBound and minBound for Integer aren't defined - infinite cardinality?
-- Cardinality of Int8 being 256 and the 8 in the bits of the Int type are equivalent to 2^8
