module PoemLines where

myWords :: String -> [String]
myWords [] = []
myWords (s:str)
  | s == ' ' = myWords str
  | otherwise = takeWhile (/= ' ') (s:str) : myWords (dropWhile (/= ' ') (s:str))

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"
secondSen :: String
secondSen = "In the forests of the night\n"
thirdSen :: String
thirdSen = "What immortal hand or eye\n"
fourthSen :: String
fourthSen = "Could frame thy fearful symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines (word:sentence)
  | word == '\n' = myLines sentence
  | otherwise =
    takeWhile (/= '\n') (word:sentence) : myLines (dropWhile (/= '\n') (word:sentence))

shouldEqual :: [String]
shouldEqual =
  ["Tyger Tyger, burning bright"
  ,"In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

mySplitOn :: String -> Char -> [String]
mySplitOn [] _ = []
mySplitOn (s:str) delim
  | s == delim = mySplitOn str delim
  | otherwise =
    takeWhile (/= delim) (s:str) : mySplitOn (dropWhile (/= delim) (s:str)) delim

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

