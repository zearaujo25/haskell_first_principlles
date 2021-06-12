{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
--this is an old impl, it doesn exist anymore
newtype Reader r a = Reader { runReader :: r -> a }


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
                    humanName :: HumanName
                    , dogName :: DogName
                    , address :: Address
                    } deriving (Eq, Show)
data Dog = Dog {
                dogsName :: DogName
                , dogsAddress :: Address
                } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person 
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- Dog:: DogName-> Address->Dog
-- dogName:: Person->DogName == 
-- address:: Person->Address 
-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f<$>fa<*>fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
    name <- rDogName
    addy <- rAddress
    return $ Dog name addy
    where rDogName = Reader dogName
          rAddress = Reader address

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ \r -> a 
    (<*>) :: Reader r (a -> b) -> Reader r a  -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r