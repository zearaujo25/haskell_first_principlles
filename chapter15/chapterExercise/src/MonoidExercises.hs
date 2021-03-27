module MonoidExercises where 

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
mempty = Trivial
mappend a b = Trivial 