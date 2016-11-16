module Ask where

newtype Reader r a = Reader { runReader :: r -> a }


ask :: Reader a a
ask = Reader id -- because runReader is now a -> a which is only id
