{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta


stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop


-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "1"

pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'

oneEOF = one >> eof

oneTwoEOF = oneTwo >> eof

test1 = do
  pNL "oneEOF succeeds parsing '1':"
  print $ parseString oneEOF mempty "1"
  pNL "oneEOF fails parsing '12':"
  print $ parseString oneEOF mempty "12"
  pNL "oneTwoEOF succeeds parsing '12':"
  print $ parseString oneTwoEOF mempty "12"
  pNL "oneTwoEOF fails parsing '1':"
  print $ parseString oneTwoEOF mempty "1"

-- read a string "1"
oneS:: Parser String
oneS = string "1"

-- read a string "12"
oneTwoS:: Parser String
oneTwoS = string "12"

-- read a string "123"
oneTwoTreeS:: Parser String
oneTwoTreeS = string "123"


test2 = do
  pNL "oneTwoTreeS succeeds parsing '123':"
  print $ parseString oneS mempty "123"


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    denominator <- decimal
    return (numerator % denominator)

rationalParser :: IO ()
rationalParser = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
    let virtuousFraction' = parseString virtuousFraction mempty
    print $ virtuousFraction' badFraction
    print $ virtuousFraction' alsoBad
    print $ virtuousFraction' shouldWork
    print $ virtuousFraction' shouldAlsoWork


type IntegerOrFraction = Either Rational Integer

intRationalParser:: Parser IntegerOrFraction
intRationalParser = do 
    v <- try ((Left <$> virtuousFraction)) <|> (Right <$> integer) 
    return v

intParser = do 
    i <- integer
    _ <- eof 
    return i





testIntParser:: IO ()
testIntParser = do 
    pNL "Integer for '123':"
    print $ parseString intParser mempty "123"
    pNL "Integer for '123abc':"
    print $ parseString intParser mempty "123abc"

testIntRationalParser:: IO ()
testIntRationalParser = do 
    pNL "Integer for '123':"
    print $ parseString intRationalParser mempty "123"
    pNL "Rational for '1/2':"
    print $ parseString intRationalParser mempty "1/2"