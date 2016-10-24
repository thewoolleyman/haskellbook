module Bind where

import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f
-- This is cheating:
-- bind = (=<<)

