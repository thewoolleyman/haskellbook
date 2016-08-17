module Programmers where

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage
             }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  let
    allCombinations :: [(OperatingSystem, ProgrammingLanguage)]
    allCombinations = [(os, lang) | os <- allOperatingSystems, lang <- allLanguages]

    tupleToProgrammer :: (OperatingSystem, ProgrammingLanguage) -> Programmer
    tupleToProgrammer (os, lang) = Programmer os lang
  in
    foldr (\t acc -> tupleToProgrammer t : acc) [] allCombinations
