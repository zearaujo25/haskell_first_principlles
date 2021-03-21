module First' where 
import Optional
import Test.QuickCheck

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
   mempty = First'{getFirst'=Nada} 

instance  Semigroup (First' a) where
     First'{getFirst'=(Only a)} <> _  = First'{getFirst'=(Only a)}
     First'{getFirst'=Nada} <> First'{getFirst'=(Only b)} = First'{getFirst'=(Only b)}
     First'{getFirst'=Nada} <> First'{getFirst'=Nada} = First'{getFirst'=Nada} 

first'Gen :: Arbitrary a => Gen (First' a) 
first'Gen = do
    a <- arbitrary
    return (First'{getFirst' = a})

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary =  first'Gen