module FunctionWithLambda where

printInc3 n =
  (\plusTwo -> print plusTwo) (n + 2)
