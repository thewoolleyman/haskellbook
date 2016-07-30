module Arith3Broken where

main :: IO ()
main = do
    print (1 + 2)
    putStrLn (show 10)
    print (negate (-1))
    let blah = negate 1 in print ((+) 0 blah)
