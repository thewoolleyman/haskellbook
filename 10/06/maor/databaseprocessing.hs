module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\x acc -> 
                      case x of 
                        DbDate date -> date : acc
                        _ -> acc
                   ) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\x acc -> 
                        case x of 
                        DbNumber number -> number : acc
                        _ -> acc
                       ) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = nom / denom
  where nom = fromIntegral $ sumDb theDatabase
        denom = fromIntegral $ length $ filterDbNumber theDatabase
  
