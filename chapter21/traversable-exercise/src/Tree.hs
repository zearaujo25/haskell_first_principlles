{-# LANGUAGE FlexibleContexts #-}
module Tree where
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node na a nb) = Node (f<$>na) (f a) (f<$>nb)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node na a nb) = (foldMap f na)<>(f a)<>(foldMap f nb)

instance Traversable Tree where
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf<$>(f a)
    traverse f (Node na a nb) = Node<$>(traverse f na)<*>(f a)<*>(traverse f nb)
    
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a  <- arbitrary
        t  <- arbitrary
        t' <- arbitrary
        frequency [(1, return Empty),
                   (3, return $ Leaf a),
                   (3, return $ Node t a t')]
instance (Eq a) => EqProp (Tree a) where (=-=) = eq