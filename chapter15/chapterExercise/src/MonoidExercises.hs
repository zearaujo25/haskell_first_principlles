module MonoidExercises where 

import Test.QuickCheck hiding (Failure,Success)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>) 

trivialGen :: Gen Trivial 
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary =  trivialGen

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a<>b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity(mempty)
    mappend = (<>)     

identityGen :: Arbitrary a => Gen (Identity a)  
identityGen = do 
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary =  identityGen

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a<>a') (b<>b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two (mempty) (mempty)
    mappend = (<>)   

twoGen :: (Arbitrary a,Arbitrary b) => Gen (Two a b)  
twoGen = do 
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =  twoGen

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b , Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a<>a') (b<>b') (c<>c')

instance (Monoid a, Monoid b , Monoid c) => Monoid (Three a b c) where
    mempty = Three (mempty) (mempty) (mempty)
    mappend = (<>)   


threeGen :: (Arbitrary a,Arbitrary b, Arbitrary c) => Gen (Three a b c)  
threeGen = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary =  threeGen

data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b , Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') = Four (a<>a') (b<>b') (c<>c') (d<>d')

instance (Monoid a, Monoid b , Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four (mempty) (mempty) (mempty) (mempty)
    mappend = (<>)  

fourGen :: (Arbitrary a,Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)  
fourGen = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary =  fourGen

newtype BoolConj = BoolConj Bool deriving(Eq,Show)
instance Semigroup BoolConj where
    (BoolConj False)   <>     (BoolConj _)       = (BoolConj False)
    (BoolConj _)       <>     (BoolConj False)   = (BoolConj False)
    (BoolConj True)    <>     (BoolConj True)    = (BoolConj True)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)  

boolConjGen :: Gen BoolConj
boolConjGen = do 
    bool <- arbitrary
    return (BoolConj bool)

instance Arbitrary (BoolConj) where
    arbitrary = boolConjGen


newtype BoolDisj = BoolDisj Bool deriving(Eq,Show)
instance Semigroup BoolDisj where
    (BoolDisj True)    <>     (BoolDisj _)       = (BoolDisj True)
    (BoolDisj _)       <>     (BoolDisj True)    = (BoolDisj True)
    (BoolDisj False)   <>     (BoolDisj False)   = (BoolDisj False)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)  

boolDisjGen :: Gen BoolDisj
boolDisjGen = do 
    bool <- arbitrary
    return (BoolDisj bool)

instance Arbitrary (BoolDisj) where
    arbitrary = boolDisjGen

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine{unCombine=f} <> Combine{unCombine=g} = Combine$ \n -> (f n) <> (g n) 

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine$ \n ->  mempty 
    mappend = (<>)  

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return (Combine f)

newtype Mem s a =
    Mem {
    runMem :: s -> (a,s)
    }

instance (Semigroup b) => Semigroup (Mem a b) where
    Mem {runMem=f} <> Mem {runMem=g}= Mem$ \s -> ((fst.f$s) <> (fst.g$s),s)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem$ \s -> (mempty,s)
    mappend = (<>)

instance Show (Mem s a) where
  show _ = "Mem a b"

instance (CoArbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
    arbitrary = do
        f <- arbitrary
        return (Mem$ \s-> ((f s), s))
