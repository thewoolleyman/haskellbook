module RShow where

import Control.Monad.Trans.Reader
import Control.Monad.Identity

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show
