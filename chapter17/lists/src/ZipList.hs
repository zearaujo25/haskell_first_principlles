module ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f as = fold (\a -> \lb -> (Cons (f a) lb)) Nil as

instance Applicative List where
    pure a = Cons a Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons hf tf) (Cons h t) = append (Cons (hf h) (hf<$>t)) (tf<*>(Cons h t))

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

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
                where xs' = let (ZipList' l) = xs
                            in take' 3000 l
                      ys' = let (ZipList' l) = ys
                            in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Arbitrary a  => Arbitrary (ZipList' a) where
  arbitrary = do
      l <- arbitrary
      return$ ZipList' l


distributeFunctions :: List (a->b) -> List a -> List b
distributeFunctions Nil _ = Nil
distributeFunctions _ Nil = Nil
distributeFunctions (Cons f fs) (Cons a as) = Cons (f a) (distributeFunctions fs as)

instance Applicative ZipList' where
    pure a = ZipList'$ repeat' a
    (<*>) (ZipList' fs) (ZipList' as)= ZipList'$ distributeFunctions fs as 

