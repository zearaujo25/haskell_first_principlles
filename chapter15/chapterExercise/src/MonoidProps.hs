module MonoidProps where 
import MonoidExercises (Combine,unCombine,Mem,runMem)

monoidLeftIdentity :: (Eq m, Monoid m)  => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

combineMonoidLeftIdentity :: String -> Combine String String -> Bool
combineMonoidLeftIdentity m comb =   (unCombine comb) m  == (unCombine (comb <> mempty)) m

combineMonoidRightIdentity :: String -> Combine String String -> Bool
combineMonoidRightIdentity m comb = (unCombine (comb <> mempty)) m == (unCombine comb) m

memMonoidLeftIdentity :: String -> Mem String String -> Bool
memMonoidLeftIdentity m mem =   (runMem mem) m  == (runMem (mem <> mempty)) m

memMonoidRightIdentity :: String -> Mem String String -> Bool
memMonoidRightIdentity m mem = (runMem (mem <> mempty)) m == (runMem mem) m