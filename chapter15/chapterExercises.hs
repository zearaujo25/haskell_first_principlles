data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
_ <> _ = undefined