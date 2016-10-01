module FixerUpper where

one :: Maybe String
one = const <$> Just "Hello" <*> pure "World"

two :: (Num t, Num a1, Num a) => Maybe (a1, a, [Char], [t])
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]