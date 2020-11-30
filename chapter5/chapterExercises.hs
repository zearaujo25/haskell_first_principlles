-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- module DetermineTheType where
-- -- simple example
-- example = 1

-- -- x = 5
-- -- y = x + 5
-- -- w = y * 10
-- -- z y = y * 10
-- -- f = 4 / y

-- -- x = "Julie"
-- -- y = " <3 "
-- -- z = "Haskell"
-- -- f = x ++ y ++ z


-- --doesnt compile
-- -- bigNum = (^) 5 $ 10
-- -- wahoo = bigNum $ 10

-- --compile
-- -- x = print
-- -- y = print "woohoo!"
-- -- z = x "hello world"

-- --doesnt compile
-- -- a = (+)
-- -- b = 5
-- -- c = b 10
-- -- d = c 200


-- a = 12 + b
-- b = 10000 * c


-- functionC ::  (Ord a, Ord b) => a -> b -> Bool
-- functionC x y = if (x > y) then True else False


module Sing where
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"
sing = if (x < y) then fstString x else sndString y
          where x = "Singin"
                y = "Somewhere"