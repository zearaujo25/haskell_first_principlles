module Main where

import System.Exit (exitSuccess)
import VigenereCipher
import CeasarCipher

main :: IO ()
main = do 
    putStrLn "Type the word to be ciphered or ceasar deciphered"
    word <- getLine
    putStrLn "Type 1 for Ceasar Cipher, 2 for Vigenere Cipher or 3 for ceasar decipher"
    choice <- getLine
    case (choice) of 
        "1" -> do 
                putStr "Type number key: "
                key <- getLine
                putStrLn (ceasarCipher word (read key :: Int))
        "2" -> do 
                putStr "Type word key: "
                key <- getLine
                putStrLn (vigenereCipher word key)
        "3" -> do 
                putStr "Type number key: "
                key <- getLine
                putStrLn (ceasarDecipher word  (read key :: Int))
        _   -> do 
                putStr "Not an available choice"
                exitSuccess