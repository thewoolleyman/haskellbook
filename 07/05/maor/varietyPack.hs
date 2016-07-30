module VarietyPack where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

k :: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k ((4 -1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Integer
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
