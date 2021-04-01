module Main where
import Test.Hspec
import Test.QuickCheck
import SemiGroupExercises
import SemiGroupProps

type TrivialMappend = Trivial -> 
                      Trivial -> 
                      Trivial -> Bool

type IdentityMappend = Identity String -> 
                       Identity String -> 
                       Identity String -> Bool

type TwoMappend = Two String String -> 
                  Two String String-> 
                  Two String String-> Bool

type ThreeMappend = Three String String String -> 
                    Three String String String -> 
                    Three String String String -> Bool

type FourMappend = Four String String String String-> 
                   Four String String String String-> 
                   Four String String String String -> Bool

type BoolConjMappend = BoolConj -> 
                       BoolConj ->
                       BoolConj -> Bool

type BoolDisjMappend = BoolDisj -> 
                       BoolDisj ->
                       BoolDisj -> Bool

type OrMappend =  Or String String -> 
                  Or String String-> 
                  Or String String-> Bool

type CombineMappend = String->
                      Combine String String -> 
                      Combine String String -> 
                      Combine String String -> Bool

type ValidationMappend =  Validation String String -> 
                          Validation String String-> 
                          Validation String String-> Bool


main :: IO ()
main = hspec $ do
    describe "Testing Associativity" $ do
        it "Trivial should hold associativity" $ do
            property $ (semiGroupAssoc :: TrivialMappend)   
        it "Identity should hold associativity" $ do
            property $ (semiGroupAssoc :: IdentityMappend)   
        it "Two should hold associativity" $ do
            property $ (semiGroupAssoc :: TwoMappend)   
        it "Three should hold associativity" $ do
            property $ (semiGroupAssoc :: ThreeMappend)   
        it "Four should hold associativity" $ do
            property $ (semiGroupAssoc :: FourMappend)   
        it "BoolConj should hold associativity" $ do
            property $ (semiGroupAssoc :: BoolConjMappend)   
        it "BoolDisj should hold associativity" $ do
            property $ (semiGroupAssoc :: BoolDisjMappend)   
        it "Or should hold associativity" $ do
            property $ (semiGroupAssoc :: OrMappend)  
        it "Combine should hold associativity" $ do
            property $ (combineSemigroupAssoc :: CombineMappend)   
        it "Validation should hold associativity" $ do
            property $ (semiGroupAssoc :: ValidationMappend)   