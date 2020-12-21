import Data.Char

filterUppers :: String -> String
filterUppers = filter isUpper 

capFirst :: String -> String
capFirst (x:xs) = (toUpper x):xs 

capString :: String -> String
capString [] = []
capString (x:xs) = (toUpper x):(capString xs) 

capFirstLetter :: String -> Char
capFirstLetter string = toUpper (head string)

capFirstLetterComp :: String -> Char
capFirstLetterComp = toUpper.head