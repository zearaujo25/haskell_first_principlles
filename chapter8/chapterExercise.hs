cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

sumN :: (Eq a, Num a) => a -> a
sumN num  = go num 0
    where go n sum
            | n == 0  = sum
            | otherwise = go (n - 1) (sum + n)

sumNRecursive :: (Eq a, Num a) => a -> a
sumNRecursive 0 = 0
sumNRecursive n = n + sumNRecursive (n -1)


multSum :: (Integral a) => a -> a -> a
multSum f1 f2  = go f1 f2 0
    where go f1 f2 sum
            | f2 == 0  = sum
            | otherwise = go f1 (f2-1) (sum + f1)

multSumRecursive :: (Integral a) => a -> a -> a
multSumRecursive _ 0  = 0
multSumRecursive f1 f2  = f1 + (multSumRecursive f1 (f2-1))



data DividedResult = Result (Integer,Integer) | DividedByZero deriving Show

dividedByCorrect :: Integer -> Integer -> DividedResult
dividedByCorrect num denom = go num denom 0 1
    where go n d count signal
            | d == 0 = DividedByZero
            | d < 0 && n < 0 = go (abs n) (abs d) count 1
            | d < 0 || n < 0 = go (abs n) (abs d) count (-1)
            | n < d = Result (count * signal,n)
            | otherwise = go (n - d) d (count + 1) signal


mc91 :: Integer -> Integer
mc91 n = if n > 100 then n-10 else mc91.mc91$n+11