module CeasarCipher (ceasarCipher,ceasarDecipher)where
import Data.Char


uppers = ['A'..'Z']
lowers = ['a'..'z']

ceasarCipher :: [Char] -> Int -> String
ceasarCipher [] _ = []
ceasarCipher (head:tail) shift 
    | (elem head lowers) = (shiftChar head shift 97) : (ceasarCipher tail shift) 
    | (elem head uppers) = (shiftChar head shift 65) : (ceasarCipher tail shift) 
    | otherwise = head:(ceasarCipher tail shift) 

ceasarDecipher :: [Char] -> Int -> String
ceasarDecipher [] _ = []
ceasarDecipher (head:tail) shift 
    | (elem head lowers) = (shiftChar head (-shift) 97) : (ceasarDecipher tail shift) 
    | (elem head uppers) = (shiftChar head (-shift) 65) : (ceasarDecipher tail shift) 
    | otherwise = head:(ceasarDecipher tail shift) 


shiftChar :: Char -> Int-> Int->Char
shiftChar  = \char -> \shift-> \reference -> chr.(+ reference)$ mod (((ord char) + shift) - reference) 26 