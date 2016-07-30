module Print3Broken where

printSecond :: IO ()
printSecond = do
    putStrLn greeting

greeting :: String
greeting = "Yarrrrrrr"

main :: IO ()
main = do
    putStrLn greeting
    printSecond
