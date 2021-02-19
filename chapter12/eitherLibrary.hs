lefts' :: [Either a b] -> [a]
lefts' = foldr leftChecker []

leftChecker :: Either a b -> [a] -> [a]
leftChecker (Right _) xs = xs
leftChecker (Left x) xs = x:xs 

rights' :: [Either a b] -> [b]
rights' = foldr rightChecker []

rightChecker :: Either a b -> [b] -> [b]
rightChecker (Left _) xs = xs
rightChecker (Right x) xs = x:xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partitionChecker ([],[])

partitionChecker :: Either a b -> ([a], [b]) -> ([a], [b])
partitionChecker (Left x) (as,bs) = (x:as,bs) 
partitionChecker (Right x) (as,bs) = (as,x:bs) 


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b


eitherMaybe'' :: (b -> c)  -> Either a b  -> Maybe c
eitherMaybe'' f var = either' (\a-> Nothing) (\b-> Just (f b)) var