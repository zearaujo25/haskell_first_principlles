module SemiGroupProps where
import SemiGroupExercises (Combine,unCombine)
semiGroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

combineSemigroupAssoc :: String -> 
                         (Combine String String)  -> 
                         (Combine String String)  -> 
                         (Combine String String)  -> 
                          Bool

combineSemigroupAssoc x a b c  =
  unCombine(((a <> b) <> c)) x  == (unCombine (a <> (b <> c))) x
