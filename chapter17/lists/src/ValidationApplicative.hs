module ValidationApplicative where
import Test.QuickCheck (Arbitrary,arbitrary,oneof)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Failure e | Success a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure e)= Failure e
    fmap f (Success a)= Success (f a)

-- This is different
instance Monoid e => Applicative (Validation e) where
    pure f = Success f
    (<*>) (Failure fErr) (Failure aErr) = Failure (fErr <> aErr)
    (<*>) _ (Failure a)  = (Failure a)
    (<*>) (Failure f) _  = (Failure f)
    (<*>) (Success f) (Success a)  = Success (f a)

instance (EqProp a, Eq a,EqProp b, Eq b) =>  EqProp (Validation a b)  where 
    (=-=) = eq


instance (Arbitrary a,Arbitrary b)  => Arbitrary (Validation a b) where
  arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return$ Failure a, return$ Success b]



