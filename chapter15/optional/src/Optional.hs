module Optional where

import Test.QuickCheck

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
   mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only a) = Only a
    (Only a) <> Nada = Only a
    (Only a) <> (Only a') = Only (a <> a')
    Nada <> Nada = Nada

optionalGen :: Arbitrary a => Gen (Optional a) 
optionalGen = do 
    a<- arbitrary
    oneof [return$Nada,return$(Only a)]

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = optionalGen