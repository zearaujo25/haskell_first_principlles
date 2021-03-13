module VigenereCipher (vigenereCipher) where
import Data.Char


uppers = ['A'..'Z']
lowers = ['a'..'z']

vigenereCipher :: [Char] -> [Char] -> String
vigenereCipher [] _ = []
vigenereCipher (x:xs) key = go x xs 0 []
    where go h t position encrypted
            | t == [] = encrypted ++ ((shiftChar h key position):[])
            | otherwise = go (head t) (tail t) (getNewPosition h position) (encrypted ++ ((shiftChar h key position):[]))

getNewPosition:: Char -> Int -> Int
getNewPosition char position =  if (elem char uppers) || (elem char lowers) then position + 1 else position 

shiftChar :: Char -> [Char]-> Int->Char
shiftChar char key position 
    |(elem char lowers) = chr.(+ 97)$ mod (((ord char) + (getCharShift key (keyPosition key position)) - 97)) 26
    |(elem char uppers) = chr.(+ 65)$ mod (((ord char) + (getCharShift key (keyPosition key position)) - 65)) 26
    |otherwise = char

keyPosition :: [Char] -> Int -> Int
keyPosition key position =  mod position (length key)

getCharShift :: [Char] -> Int -> Int
getCharShift key position = ord (key!!position) -65