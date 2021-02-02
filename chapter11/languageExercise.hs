import Data.Char

capitalizeWord :: String -> String
capitalizeWord (' ':xs) =  " "++(capitalizeWord xs)
capitalizeWord (x:xs) =  (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p =(capitalizeWord (takeWhile (/= '.') p)) ++ "." ++ (capitalizeParagraph.tail.(dropWhile (/= '.'))$p)

capitalizeParagraph' :: String -> String
capitalizeParagraph' [] = []
capitalizeParagraph' phrase = go phrase True [] 
        where go p isNewSentence capitalized
                | (tail p) == [] = capitalized ++ [(head p)]
                | (head p) == ' ' = go (tail p) isNewSentence (capitalized ++ [(head p)])
                | (head p) == '.' = go (tail p) True (capitalized ++ [(head p)])
                | isNewSentence = go (tail p) False (capitalized ++ [(toUpper (head p))])
                | otherwise = go (tail p) False (capitalized ++ [(head p)])

capitalizeParagraph'' :: String -> String
capitalizeParagraph'' [] = []
capitalizeParagraph'' (x:xs) = go x xs True [] 
        where go h t isNewSentence capitalized
                | t == [] = capitalized ++ [h]
                | h == ' ' = go (head t) (tail t) isNewSentence (capitalized ++ [h])
                | h == '.' = go (head t) (tail t) True (capitalized ++ [h])
                | isNewSentence = go (head t) (tail t) False (capitalized ++ [(toUpper h)])
                | otherwise = go (head t) (tail t)  False (capitalized ++ [h])