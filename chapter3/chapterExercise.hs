module Reverse where

thirdLetter :: String->Char 
thirdLetter x = (!!) x 3

letterIndex :: Int -> Char
letterIndex n = (!!) x n
             where x="Curry is awesome!"

rvs:: String
rvs = concat [awesome, is, curry ]
             where currys_string = "Curry is awesome"
                   awesome = drop 9 currys_string
                   is = drop 5 $ take 9 currys_string
                   curry = take 5 currys_string
main :: IO ()
main = print (rvs)