
fibs = 1 : scanl (+) 1 fibs
fibs' = take 20 fibs
fibs'' = takeWhile  (<100) fibs

doFactorial :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
doFactorial (accFact,accN) (currentFac,currentN) = (accFact * (accN+1),accN+1 )
internalFactorial = scanl doFactorial (1,1) internalFactorial
factorial n = (map (fst)).(take n)$internalFactorial


doFactorial' :: Integer -> Integer -> Integer
doFactorial' acc current = acc * ( (+) 1 (div acc current))
internalFactorial' = 1:scanl doFactorial' 2 internalFactorial'
factorial' n = take n internalFactorial'

