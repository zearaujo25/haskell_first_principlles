{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}
module IntegerParser where
import Control.Applicative
import Data.Char (isAlpha)
import Text.Read
import Data.Maybe
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['1' .. '9']

base10Integer :: Parser Integer
base10Integer = do 
    signal <- option "+" (string "-")
    stringNumber <- some parseDigit
    return$ (read stringNumber :: Integer) * (read (signal ++ "1") :: Integer)