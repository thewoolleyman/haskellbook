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

-- Yep, just what it sounds like ¯\_(ツ)_/¯
parseNOSes :: Char -> Parser [NumberOrString]
parseNOSes a = do
  _ <- char a
  p <- parseNOS
  ps <- many $ char '.' >> parseNOS
  return $ p : ps

parseNOS :: Parser NumberOrString
parseNOS =
  fmap NOSI integer <|> fmap NOSS (many letter)

parseSemVer :: Parser SemVer
parseSemVer = 
  SemVer
    <$> integer
    <*> (char '.' >> integer)
    <*> (char '.' >> integer)
    <*> (fmap (const []) eof <|> parseNOSes '-')
    <*> (fmap (const []) eof <|> parseNOSes '+')


main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  --print $ parseString parseSemVer mempty "1.0.0+20130313144700"
  print $ parseString parseSemVer mempty "1.0.0-beta+exp.sha.5114f85"
  --SemVer 2 1 1 [] [] > SemVer 2 10 [] []
