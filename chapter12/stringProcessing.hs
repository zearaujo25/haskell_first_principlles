import Data.Char

vowels = ['a','e','i','o','u']

notThe :: String -> Maybe String
notThe word  
    | word == "the" = Nothing
    | otherwise = Just word

-- replaceThe :: String -> String
replaceThe  =    putTogether.(map notThe).words 


putTogether :: [Maybe String] -> String
putTogether [] = []
putTogether ((Just word):[]) = word
putTogether (Nothing:[]) = "a"
putTogether (Nothing:xs) = "a " ++ (putTogether xs)
putTogether ((Just word):xs) = word ++ " " ++ (putTogether xs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = sumVowelThe.(map notThe).words

sumVowelThe :: [Maybe String] -> Integer
sumVowelThe [] =  0
sumVowelThe (x:[]) = 0
sumVowelThe (x1:x2:xs) = (theChecker x1 x2) + (sumVowelThe (x2:xs))


theChecker :: Maybe String -> Maybe String -> Integer
theChecker Nothing (Just word) = if elem (toLower.head$word) vowels then 1 else 0
theChecker _ _ = 0



countVowels :: String -> Int
countVowels = length.(filter isVowell).(map toLower)

isVowell :: Char -> Bool
isVowell =  (flip elem) vowels 



newtype Word' = Word' String deriving (Eq, Show)


mkWord :: String -> Maybe Word'
mkWord word= wordChecker.(foldr countVowelConsonants (0,0,word)).(map toLower)$word

countVowelConsonants :: Char -> (Int,Int,String) -> (Int,Int,String)
countVowelConsonants char (nVowel,nConsonant,word) = if isVowell char then (nVowel+1,nConsonant,word) else (nVowel,nConsonant+1,word)

wordChecker :: (Int,Int,String) -> Maybe Word'
wordChecker (nVowel,nConsonant,word)
            | nVowel > nConsonant = Nothing
            | otherwise = Just (Word' word)