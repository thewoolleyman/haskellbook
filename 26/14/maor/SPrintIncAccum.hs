module SPrintIncAccum where

import Control.Monad.Trans.State
import Data.Monoid

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = 
  StateT $ 
    \a -> putStrLn  ("Hi: " <> show a) >> return  (show a, a + 1)
