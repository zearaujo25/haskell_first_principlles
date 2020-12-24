import Data.Char

filterUppers :: String -> String
filterUppers = filter isUpper 

capFirst :: String -> String
capFirst (x:xs) = (toUpper x):xs 

capString :: String -> String
capString [] = []
capString (x:xs) = (toUpper x):(capString xs) 

capFirstLetter :: String -> Char
capFirstLetter string = toUpper (head string)

capFirstLetterComp :: String -> Char
capFirstLetterComp = toUpper.head

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem element (x:xs) = x == element || myElem element xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny element list = any (element==) list


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ x:[]


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] =[]
squishMap f (x:xs) = f x ++ squishMap f xs


squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap (\x-> [x]) x ++ squishAgain xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs)= go x x xs
    where go element max xs
            | length xs == 0 = if (f element max) == GT then element else max
            | (f element max) == GT = go (head xs) element (tail xs)
            | otherwise = go (head xs) max (tail xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs)= go x x xs
    where go element min xs
            | length xs == 0 = if (f element min) == LT then element else min
            | (f element min) == LT = go (head xs) element (tail xs)
            | otherwise = go (head xs) min (tail xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum list = myMaximumBy compare list

myMinimum :: (Ord a) => [a] -> a
myMinimum list = myMinimumBy compare list