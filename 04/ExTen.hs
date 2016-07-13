module ExTen where

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f tupleOne tupleTwo = ((snd tupleOne, snd tupleTwo), (fst tupleOne, fst tupleTwo))