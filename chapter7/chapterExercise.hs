tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10 
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = divMod x 10
        divResult = fst xLast
        yLast = divMod divResult 10
        d = snd yLast

hunsD x = d2
  where xLast = divMod x 100
        divResult = fst xLast
        yLast = divMod divResult 100
        d2 = snd yLast

foldBool :: a -> a -> Bool -> a
foldBool x y choice = case choice of 
                        True -> y
                        False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y choice 
    | choice == True = y
    | choice == False = x

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a,c) = (aToB a,c)
    