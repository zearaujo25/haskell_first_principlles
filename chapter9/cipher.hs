module Cipher where
import Data.Char


uppers = ['A'..'Z']
lowers = ['a'..'z']

cipher :: [Char] -> Int -> String
cipher [] _ = []
cipher (head:tail) shift 
    | (elem head lowers) = (shiftChar head shift 97) : (cipher tail shift) 
    | (elem head uppers) = (shiftChar head shift 65) : (cipher tail shift) 
    | otherwise = head:(cipher tail shift) 

decipher :: [Char] -> Int -> String
decipher [] _ = []
decipher (head:tail) shift 
    | (elem head lowers) = (shiftChar head (-shift) 97) : (decipher tail shift) 
    | (elem head uppers) = (shiftChar head (-shift) 65) : (decipher tail shift) 
    | otherwise = head:(decipher tail shift) 


shiftChar :: Char -> Int-> Int->Char
shiftChar  = \char -> \shift-> \reference -> chr.(+ reference)$ mod (((ord char) + shift) - reference) 26 