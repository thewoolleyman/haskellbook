module Parametricity55b where

x :: a -> a -> a
x (==) (/=) = (==)

y :: a -> a -> a
y (/=) (==) = (==)

xx :: a -> a -> a
xx (==) (/=) = (/=)

yy :: a -> a -> a
yy (==) (/=) = (==)
