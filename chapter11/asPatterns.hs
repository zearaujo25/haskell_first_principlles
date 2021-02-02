import Data.Char

isSubseqOf :: (Eq a)=> [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf [] _ = True
isSubseqOf l@(x:xs) m@(y:ys) = (x==y || (isSubseqOf l ys)) && (isSubseqOf xs m)


capitalizeWords :: String -> [(String, String)]
capitalizeWords [] =  []
capitalizeWords xs =  (map (\l@(x:xs)->(l,(toUpper x):xs))).words$xs