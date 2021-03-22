module ChapterExercises where
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

trivialGen :: Gen Trivial 
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary =  trivialGen

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a<>b)

identityGen :: Arbitrary a => Gen (Identity a)  
identityGen = do 
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary =  identityGen

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a<>a') (b<>b')

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

boolDisjGen :: Gen BoolDisj
boolDisjGen = do 
    bool <- arbitrary
    return (BoolDisj bool)

instance Arbitrary (BoolDisj) where
    arbitrary = boolDisjGen

data Or a b = Fst a | Snd b deriving(Eq,Show)
instance Semigroup (Or a b) where
    (Fst _)    <>     b       = b
    (Snd a)    <>     _       = (Snd a) 


orGen :: (Arbitrary a,Arbitrary b) => Gen (Or a b)  
orGen = do 
    a <- arbitrary
    b <- arbitrary
    oneof [return$ (Fst a), return$(Fst b)]

instance (Arbitrary a,Arbitrary b) => Arbitrary (Or a b) where
    arbitrary =  orGen