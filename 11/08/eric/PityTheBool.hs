module PityTheBool where

import Data.Int

-- Cardinality = 4 - Big Bool = 2, Small Bool = 2, Sum type = Big Bool + Small Bool
data BigSmall = Big Bool
              | Small Bool


-- Cardinality = 258 - Int8 = 256 + Bool = 2
data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

-- Throws a warning
let myNumba = Numba (-128)
