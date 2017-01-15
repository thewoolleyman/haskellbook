module Rdec where

import Control.Monad.Trans.Reader
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity .  flip (-) 1
