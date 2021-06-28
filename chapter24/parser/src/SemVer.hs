{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}
module SemVer where
import Control.Applicative
import Data.Char (isAlpha)
import Text.Read
import Data.Maybe
import Text.Trifecta
-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
      NOSS String
    | NOSI Integer
    deriving (Show)


type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show)
parseSemVer :: Parser SemVer
parseSemVer = do
    major <-integer
    _ <- char '.'
    minor <- integer
    _ <- char '.'
    patch <- integer
    skipOptional (char '-') 
    release <- option [] parseDots
    skipOptional (char '+') 
    metadata <- option [] parseDots
    eof
    return (SemVer major minor patch release metadata)

parseDots:: Parser [NumberOrString]
parseDots = sepBy nosiOrNOSSParser (symbol ".")


nosiOrNOSSParser::Parser NumberOrString
nosiOrNOSSParser = (NOSI<$>integer) <|> (NOSS<$>some alphaNum)