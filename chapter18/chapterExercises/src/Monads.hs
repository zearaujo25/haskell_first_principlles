module Monads where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (join)

data Sum a b =First a| Second b deriving (Eq, Show) --just like PhhhbbtttEither

instance Functor (Sum a) where
    fmap f (Second b)= (Second (f b))
    fmap _ (First a)= (First a)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First f) _ = First f
    (<*>) _ (First a) = First a
    (<*>) (Second f) (Second a) = (Second (f a))

instance Monad (Sum a) where
    return = pure
    (>>=) (Second b) f = f b
    (>>=) (First a) _ = First a

instance (Arbitrary a,Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary 
        oneof [return $ First a,return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

---------------- Nope ---------------
data Nope a = NopeDotJpg deriving (Eq,Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq


---------------- Identity ---------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a)= Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do 
        a <- arbitrary 
        return $ Identity a 

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq



---------------- List ---------------
data List a = Nil | Cons a (List a) deriving (Eq,Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f as = fold (\a -> \lb -> (Cons (f a) lb)) Nil as

instance Applicative List where
    pure a = Cons a Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons hf tf) (Cons h t) = append (Cons (hf h) (hf<$>t)) (tf<*>(Cons h t))

instance Monad List where
    return = pure
    (>>=) Nil _ = Nil
    (>>=) list f = concat'$ f<$>list

instance Arbitrary a  => Arbitrary (List a) where
  arbitrary = 
    oneof [nil, cons]
    where nil = return Nil
          cons = do
            h <- arbitrary
            tl <- arbitrary
            return $ Cons h tl 

instance (EqProp a, Eq a) =>  EqProp (List a)  where 
    (=-=) = eq             

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil= b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
            -> List a
            -> List b
flatMap f as = concat'$ f<$>as  

accumulateTake :: a -> (List a, Int) -> Int -> (List a, Int)
accumulateTake a (bs,m) n = if m == n then (bs,m) else ((append (Cons a Nil) bs), m+1)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 1 (Cons a _) = Cons a Nil
take' n (Cons a xs) = if n > 0 then Cons a (take' (n-1) xs) else (Cons a xs)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a) 


j :: Monad m => m (m a) -> m a
j ms = join ms

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f<$>m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f<$>m1<*>m2

a :: Monad m => m a -> m (a -> b) -> m b
a m mf  = mf<*>m

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (h:t) f =  (++) <$> list <*> (meh t f)
                where 
                    list = (:[]) <$> (f h)
                    
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id