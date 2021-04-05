module InstancesOfFunc where
import Test.QuickCheck
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)  
identityGen = do 
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary =  identityGen

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

twoGen :: (Arbitrary a,Arbitrary b) => Gen (Two a b)  
twoGen = do 
    a <- arbitrary
    b <- arbitrary
    return (Two a b)



instance (Arbitrary a,Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =  twoGen

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a,Arbitrary b, Arbitrary c) => Gen (Three a b c)  
threeGen = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary =  threeGen

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

fourGen :: (Arbitrary a,Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)  
fourGen = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary =  fourGen

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

four'Gen :: (Arbitrary a,Arbitrary b) => Gen (Four' a b)  
four'Gen = do 
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return (Four' a a' a'' b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary =  four'Gen

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers (f a)
    fmap _ LolNope = LolNope 

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = (First a) 