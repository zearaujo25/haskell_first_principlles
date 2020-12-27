stops = "pbtdkg"
vowels = "aeiou"

nouns = ["car","kid","thunder","fruit","pumpking"] 
verbs = ["eat","fly","poop","dance","lick"] 

wordsGenerated  stops vowels = [(x,y,z) | x <- stops,y <- vowels, z<-stops]
wordsGeneratedP  stops vowels = [(x,y,z) | x <- stops,y <- vowels, z<-stops, x == 'p']
phraseGenerator nouns verbs = [(x,y,z) | x <- nouns,y <- verbs, z<-nouns]

seekritFunc x =  ( (fromIntegral.sum.(map length).words) x) / ( (fromIntegral.length.words) x)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOrFold :: [Bool] -> Bool
myOrFold values = foldr (||) False values

myOrFoldPF :: [Bool] -> Bool
myOrFoldPF = foldr (||) False 



myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myAnyFold :: (a -> Bool) -> [a] -> Bool
myAnyFold f list = foldr ((||).f) False list


--not sure 
myAnyFoldPF :: (a -> Bool) -> [a] -> Bool
myAnyFoldPF f =  foldr ((||).f)  False



myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem element (x:xs) = x == element || myElem element xs

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold element list = foldr ((||).((==) element)) False list


myElemAny :: Eq a => a -> [a] -> Bool
myElemAny element list = any (element==) list



myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ x:[]

myReverseFold:: [a] -> [a]
myReverseFold list = foldr ((flip (++)).(:[])) [] list



myMap :: (a -> b) -> [a] -> [b]
myMap f list = foldr ((++).(:[]).f) [] list



filterFuction :: (a -> Bool) -> a -> [a] -> [a]
filterFuction f elem acc = if (f elem) then elem:[] ++ acc else acc


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f list = foldr  (filterFuction f) [] list


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishFold :: [[a]] -> [a]
squishFold = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] =[]
squishMap f (x:xs) = f x ++ squishMap f xs


squishMapFold :: (a -> [b]) -> [a] -> [b]
squishMapFold f list = foldr ((++).f) [] list

squishAgain :: [[a]] -> [a]
squishAgain list = squishMapFold id list



myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs)= go x x xs
    where go element max xs
            | length xs == 0 = if (f element max) == GT then element else max
            | (f element max) == GT = go (head xs) element (tail xs)
            | otherwise = go (head xs) max (tail xs)

myMaximumByFold :: (a -> a -> Ordering) -> [a] -> a
myMaximumByFold f (x:xs) = foldr (\ele -> \acc -> if (f ele acc) == GT  then ele else acc) x xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs)= go x x xs
    where go element min xs
            | length xs == 0 = if (f element min) == LT then element else min
            | (f element min) == LT = go (head xs) element (tail xs)
            | otherwise = go (head xs) min (tail xs)


myMinimumByFold :: (a -> a -> Ordering) -> [a] -> a
myMinimumByFold f (x:xs) = foldr (\ele -> \acc -> if (f ele acc) == LT  then ele else acc) x xs