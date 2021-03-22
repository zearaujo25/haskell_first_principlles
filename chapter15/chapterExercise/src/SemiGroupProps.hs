module SemiGroupProps where

semiGroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
