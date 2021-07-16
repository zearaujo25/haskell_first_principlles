{-# LANGUAGE InstanceSigs #-}
module Compose where 

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose .pure.pure
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fgab) <*> (Compose fga) = Compose$ (<*>)<$>fgab<*>fga


instance (Foldable f, Foldable g ) => Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap.foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id
    second :: (b -> c) -> p a b -> p a c
    second = bimap id