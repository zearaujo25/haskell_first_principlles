import Data.List (elemIndex)

added :: Maybe Integer
added = (+3)<$>(lookup 3 $ zip [1, 2, 3] [4, 5, 6])
-----------------------------------------------------------------
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--------------------------------------------------------
xmax :: Maybe Int
xmax = elemIndex 3 [1, 2, 3, 4, 5]

ymax :: Maybe Int
ymax = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> xmax <*> ymax

-------------------------------------

xs = [1, 2, 3]
ys = [4, 5, 6]

xsum :: Maybe Integer
xsum = lookup 3 $ zip xs ys

ysum :: Maybe Integer
ysum = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> xsum <*> ysum)