{-# LANGUAGE FlexibleContexts #-}
module SkiFree where
import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
        , Arbitrary (n a)
        , Arbitrary a )
        => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary
--The isntance from the book is broke, switched to this one  
instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

instance (Functor n) => Functor (S n) where
    fmap f (S x y) = S (f<$>x) (f y)

instance (Applicative n) => Applicative (S n)  where
    pure a = S (pure a) a
    (<*>) (S (nf) f) (S n a) = S (nf<*>n) (f a)

instance Foldable n =>  Foldable (S n) where
    foldr f z (S x y) = foldr f (f y z) x 

instance Traversable n => Traversable (S n) where
    traverse f (S n a) =  S<$>(traverse f n)<*>(f a)