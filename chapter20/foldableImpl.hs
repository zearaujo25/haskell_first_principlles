data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f z (Constant a) = f a z

data Two a b = Two a b
instance Foldable (Two a) where
    foldr f z (Two _ a) = f a z

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldr f z (Three _ _ a) = f a z

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldr f z (Three' _ a b) = (f b).(f a)$z

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldr f z (Four' _  a b c) = (f c).(f b).(f a)$z

filterF :: ( Applicative f
            , Foldable t
            , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)