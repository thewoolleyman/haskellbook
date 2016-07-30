module ArtfulDodgy where

dodgy x y = x + y * 10
oneIsOne :: (Integer -> Integer)
oneIsOne = dodgy 1 -- returns a function that accepts one parameter and returns an Integer

oneIsTwo :: (Integer -> Integer)
oneIsTwo = (flip dodgy) 2



