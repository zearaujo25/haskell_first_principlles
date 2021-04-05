import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec
import InstancesOfFunc

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                                (a -> b)
                                -> (b -> c)
                                -> f a
                                -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                            f a
                            -> Fun a b
                            -> Fun b c
                            -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

type IdentityInt = Identity Int
type IdentityIntId = IdentityInt -> Bool
type IdentityIntFC = IdentityInt -> IntToInt -> IntToInt -> Bool

type TwoInt = Two Int Int
type TwoIntId = TwoInt -> Bool
type TwoIntFC = TwoInt -> IntToInt -> IntToInt -> Bool

type ThreeInt = Three Int Int Int
type ThreeIntId = ThreeInt -> Bool
type ThreeIntFC = ThreeInt -> IntToInt -> IntToInt -> Bool

type FourInt = Four Int Int Int Int
type FourIntId = FourInt -> Bool
type FourIntFC = FourInt -> IntToInt -> IntToInt -> Bool

type Four'Int = Four' Int Int
type Four'IntId = Four'Int -> Bool
type Four'IntFC = Four'Int -> IntToInt -> IntToInt -> Bool


main :: IO ()
main = hspec $ do
    describe "Testing Identity " $ do
        it "Indentity should hold identity" $ do
            property $ (functorIdentity :: IdentityIntId)   
        it "Indentity should hold composing" $ do
            property $ (functorCompose' :: IdentityIntFC)   

    describe "Testing Two " $ do
        it "Two should hold identity" $ do
            property $ (functorIdentity :: TwoIntId)   
        it "Two should hold composing" $ do
            property $ (functorCompose' :: TwoIntFC)  
   
    describe "Testing Three " $ do
        it "Three should hold identity" $ do
            property $ (functorIdentity :: ThreeIntId)   
        it "Three should hold composing" $ do
            property $ (functorCompose' :: ThreeIntFC)    
    
    describe "Testing Four " $ do
        it "Four should hold identity" $ do
            property $ (functorIdentity :: FourIntId)   
        it "Four should hold composing" $ do
            property $ (functorCompose' :: FourIntFC)    

    describe "Testing Four' " $ do
        it "Four' should hold identity" $ do
            property $ (functorIdentity :: Four'IntId)   
        it "Four' should hold composing" $ do
            property $ (functorCompose' :: Four'IntFC)    

