module TestTypes where

foo :: a -> a -> a
foo (==) (/=) = (==)

y :: a -> a -> a
y (/=) (==) = (/=)