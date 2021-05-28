
module LibraryFunctions  where

sum :: (Foldable t, Num a) => t a -> a
sum t = foldr (+) 0 t

product :: (Foldable t, Num a) => t a -> a
product t = foldr (*) 1 t

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a t = foldr (\x-> \y-> (a==x) || y) False t

minimumAux ::  (Ord a) => a -> Maybe a ->  Maybe a
minimumAux a Nothing = (Just a)
minimumAux a (Just b) = if (a<b) then (Just a) else (Just b)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum t = foldr minimumAux Nothing t


maximumAux ::  (Ord a) => a -> Maybe a ->  Maybe a
maximumAux a Nothing = (Just a)
maximumAux a (Just b) = if (a>b) then (Just a) else (Just b)

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum t = foldr maximumAux Nothing t

null :: (Foldable t) => t a -> Bool
null = foldr (\x-> \y-> True) False 

length :: (Foldable t) => t a -> Int
length = foldr (\_-> \y-> y+1) 0 

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x-> \y-> [x] ++ y) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (mappend mempty)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = foldr (\x-> \y-> mappend (f x) y) mempty t