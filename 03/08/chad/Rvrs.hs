-- ch 3.8, Building Functions, exercise 5
module ReturnThird where

main :: IO()
main = do
  putStrLn rvrs

rvrs :: String
rvrs =
  let
    string = "Curry is awesome"
  in
    drop 9 string ++ drop 5 (take 9 string ) ++ take 5 string
