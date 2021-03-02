module Main where

import VigenereCipher
import CeasarCipher

main :: IO ()
main = do 
    putStrLn "Type the word to be ciphered"
    word <- getLine
    putStr "Type number key: "
    key <- getLine
    putStrLn (ceasarCipher word (read key :: Int))
