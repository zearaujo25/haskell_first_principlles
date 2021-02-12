isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee fallback f Nothing = fallback

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback a = mayybee fallback id a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = (maybeToList x) ++ (catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe  = foldr maybeChecker (Just []) 

maybeChecker :: Maybe a -> Maybe [a] -> Maybe [a]
maybeChecker _ Nothing = Nothing
maybeChecker Nothing _ = Nothing
maybeChecker x (Just xs) = Just ((maybeToList x) ++ xs )
