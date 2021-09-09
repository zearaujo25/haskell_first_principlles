module Main where

import VigenereCipher

import System.Environment (getArgs)

import System.IO

import System.Exit

getKeyAndMode :: IO (String,String) 
getKeyAndMode = do 
    input <- getArgs
    return (input!!0, input!!1)




main :: IO ()
main = do 
    (key,mode) <- getKeyAndMode
    case (mode) of
        "-d" -> do 
            isAvailble <- hWaitForInput stdin 10000
            case isAvailble of
                True -> do  textToEcrypt <- hGetContents  stdin
                            hPutStr stdout (vigenereCipher textToEcrypt key) 
                False -> do 
                    hPutStr stderr "Took too long"
                    exitFailure
