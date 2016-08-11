module Zipping where

myZip :: [a] -> [b] -> [(a,b)]
myZip (x1:xs1) (x2:xs2) = (x1,x2) : myZip xs1 xs2
myZip _ [] = []
myZip [] _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x1:xs1) (x2:xs2) = f x1 x2 : myZipWith f xs1 xs2
myZipWith _ _ [] = []
myZipWith _ [] _ = []
