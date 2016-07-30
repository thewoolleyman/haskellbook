module Ch3 where

--addbang :: String -> String
addbang x = x ++ "!"

getfifth :: String -> String
getfifth y = [y !! 5]

dropninth :: String -> String
dropninth y = drop 9 y

thirdLetter :: String -> Char
thirdLetter x = x !! 2 

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x
