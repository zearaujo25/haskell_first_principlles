module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
----------Identity---------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a)= Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)
    
instance Foldable Identity where
    foldr f z (Identity a) = f a z

instance Traversable Identity where
    traverse f (Identity a) = Identity<$>(f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do 
        a <- arbitrary 
        return $ Identity a 

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

----------Constant---------
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = (Constant a) 

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant$ a<>b
    
instance Foldable (Constant a) where
    foldr _ z _ = z

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure$ (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = do 
        a <- arbitrary 
        return $ Constant a 

instance (Eq a) => EqProp (Constant a b) where
    (=-=) (Constant a) (Constant b) = eq a b

----------Maybe---------
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada = Nada 
    fmap f (Yep a) = Yep$ f a

instance Applicative Optional  where
    pure = Yep
    (<*>) Nada _ = Nada
    (<*>) _ Nada = Nada
    (<*>) (Yep f) (Yep a) = Yep$ f a

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep a) = f a z

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep<$>(f a)

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = do 
        a <- arbitrary 
        oneof [return$Nada, return$ Yep a] 

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq

----------List---------
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil = Nil 
    fmap f (Cons h t) = Cons (f h) (fmap f t)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil a = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons hf tf) (Cons h t) = append (Cons (hf h) (hf<$>t)) (tf<*>(Cons h t))

instance Foldable List where
    foldr _ z Nil = z
    foldr f z (Cons a t) = f a (foldr f z t)

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do 
        a <- arbitrary 
        b <- arbitrary
        oneof [return$Nil, return$ Cons a b] 

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

----------Three---------
data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b)  where
    pure = Three mempty mempty
    (<*>) (Three a b f) (Three a' b' c) = Three (a<>a') (b<>b') (f c)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
    traverse f (Three a b c) =  (Three a b)<$>(f c)

instance (Arbitrary a,Arbitrary b,Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do 
        a <- arbitrary 
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c 

instance (Eq a,Eq b,Eq c) => EqProp (Three a b c) where
    (=-=) = eq


----------Pair---------
data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance (Monoid a) => Applicative (Pair a)  where
    pure = Pair mempty
    (<*>) (Pair a f) (Pair a' b) = Pair (a<>a') (f b)

instance Foldable (Pair a) where
    foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where
    traverse f (Pair a b) =  (Pair a)<$>(f b)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = do 
        a <- arbitrary 
        b <- arbitrary
        return $ Pair a b 

instance (Eq a,Eq b) => EqProp (Pair a b) where
    (=-=) = eq

----------Big---------
data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance (Monoid a) => Applicative (Big a)  where
    pure a = Big mempty a a
    (<*>) (Big a f f') (Big a' b b') = Big (a<>a') (f b) (f' b')

instance Foldable (Big a) where
    foldr f z (Big a b b') = f b (f b' z)

instance Traversable (Big a) where
    traverse f (Big a b b') =  (Big a)<$>(f b)<*>(f b')

instance (Arbitrary a,Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do 
        a <- arbitrary 
        b <- arbitrary
        b'<-arbitrary
        return $ Big a b b' 

instance (Eq a,Eq b) => EqProp (Big a b) where
    (=-=) = eq

----------Bigger---------

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance (Monoid a) => Applicative (Bigger a)  where
    pure a = Bigger mempty a a a
    (<*>) (Bigger a f f' f'') (Bigger a' b b' b'') = Bigger (a<>a') (f b) (f' b') (f'' b'')

instance Foldable (Bigger a) where
    foldr f z (Bigger a b b' b'') = f b (f b' (f b'' z))

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') =  (Bigger a)<$>(f b)<*>(f b')<*>(f b'')

instance (Arbitrary a,Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = do 
        a <- arbitrary 
        b <- arbitrary
        b'<- arbitrary
        b'' <- arbitrary
        return $ Bigger a b b' b'' 

instance (Eq a,Eq b) => EqProp (Bigger a b) where
    (=-=) = eq
    
----------SkiFree---------
data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
        , Arbitrary (n a)
        , Arbitrary a )
        => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary
instance ( Applicative n
        , Testable (n Property)
        , EqProp a )
        => EqProp (S n a) where
(S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p)
    .&. (y Test.QuickCheck.Checkers.(=-=) q)