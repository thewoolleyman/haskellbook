module Parsing where

import Text.Trifecta
import Control.Applicative

data NumberOrString =
         NOSS String
       | NOSI Integer
       deriving (Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Show)

parseRelease :: Parser Release
parseRelease = do
  char '-'
  return []

parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  release <- (fmap (const []) eof <|> parseRelease)
  -- many (char '-')
  return $ SemVer maj minor patch release [(NOSS "")]


main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  --SemVer 2 1 1 [] [] > SemVer 2 10 [] []
