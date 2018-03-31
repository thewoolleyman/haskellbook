-- ch 3.8, Bulding Functions, exercise 3
module ReturnThird where

main :: IO()
main = do
  putStrLn [thirdLetter "abcd"]

thirdLetter :: String -> Char
thirdLetter string =
  head (drop 2 string)
