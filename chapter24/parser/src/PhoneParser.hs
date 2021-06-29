module PhoneParser where
import Control.Applicative
import Data.Char (isAlpha)
import Text.Read
import Data.Maybe
import Text.Trifecta

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = ( try(parsePureNumber)
            <|>try(parseThreeHifenNumber)
            <|>try(parsePatenthesisNumber)
            <|>try(parseFourHifenNumber))


parseFourHifenNumber:: Parser PhoneNumber
parseFourHifenNumber = do 
    _ <- digit 
    _ <- char '-'
    parseThreeHifenNumber

parsePatenthesisNumber:: Parser PhoneNumber
parsePatenthesisNumber = do 
    _ <- (char '(')
    numPlainArea <- parseThreeDigits
    _ <- (string ") ")
    exchange <- parseThreeDigits
    _ <- (char '-')
    lineNumber <- parseFourDigits
    eof
    return$ PhoneNumber numPlainArea exchange lineNumber


parsePureNumber:: Parser PhoneNumber
parsePureNumber = do 
    numPlainArea <- parseThreeDigits
    exchange <- parseThreeDigits
    lineNumber <- parseFourDigits
    eof
    return$ PhoneNumber numPlainArea exchange lineNumber


parseThreeHifenNumber:: Parser PhoneNumber
parseThreeHifenNumber = do 
    numPlainArea <- parseThreeDigits
    _ <- (char '-')
    exchange <- parseThreeDigits
    _ <- (char '-')
    lineNumber <- parseFourDigits
    eof
    return$ PhoneNumber numPlainArea exchange lineNumber


parseThreeDigits:: Parser Int
parseThreeDigits = do 
    d1 <- digit 
    d2 <- digit 
    d3 <- digit
    return$ (read [d1,d2,d3] :: Int)

parseFourDigits:: Parser Int
parseFourDigits = do 
    d1 <- digit 
    d2 <- digit 
    d3 <- digit
    d4 <- digit
    return$ (read [d1,d2,d3,d4] :: Int)


data List a = Cons a (List a) | Empty deriving Show 

fromList :: [a] -> List a 
fromList  = foldr (\a-> \b-> Cons a b ) Empty 


fromListReverse :: [a] -> List a 
fromListReverse  = foldr (\a-> \b-> appendList b (Cons a Empty) ) Empty 

appendList :: List a -> List a-> List a 
appendList Empty Empty = Empty
appendList a Empty = a
appendList Empty b = b 
appendList (Cons a b) c = (Cons a)$ appendList b c


