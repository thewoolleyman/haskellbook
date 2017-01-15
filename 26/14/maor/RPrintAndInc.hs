module RPrintAndInc where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Data.Monoid

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = 
  ReaderT $
    \a -> 
      putStrLn ( "hi: " <> show a) >> return ( a + 1)
