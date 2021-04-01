module Main where 
import Test.QuickCheck
import Test.Hspec
import MonoidExercises
import SemiGroupProps
import MonoidProps



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

type CombineMappend = String->
                      Combine String String -> 
                      Combine String String -> 
                      Combine String String -> Bool


-- type ValidationMappend =  Validation String String -> 
--                           Validation String String-> 
--                           Validation String String-> Bool


main :: IO ()
main = hspec $ do
    describe "Testing Monoid Properties" $ do
        it "Trivial should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: TrivialMappend)
            quickCheck  (mli :: Trivial -> Bool)
            quickCheck  (mlr :: Trivial -> Bool)
        it "Identity should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: IdentityMappend)
            quickCheck  (mli :: Identity String -> Bool)
            quickCheck  (mlr :: Identity String  -> Bool)
        it "Two should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: TwoMappend)
            quickCheck  (mli :: Two String String -> Bool)
            quickCheck  (mlr :: Two String  String-> Bool)
        it "Three should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: ThreeMappend)
            quickCheck  (mli :: Three String String String -> Bool)
            quickCheck  (mlr :: Three String String String -> Bool)
        it "Four should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: ThreeMappend)
            quickCheck  (mli :: Four String String String String -> Bool)
            quickCheck  (mlr :: Four String String String String -> Bool)
        it "BoolConj should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: BoolConjMappend)
            quickCheck  (mli :: BoolConj -> Bool)
            quickCheck  (mlr :: BoolConj-> Bool)
        it "BoolDisj should hold Monoid props" $ do
            let sa = semiGroupAssoc
            let mli = monoidLeftIdentity
            let mlr = monoidRightIdentity
            quickCheck  (sa :: BoolDisjMappend)
            quickCheck  (mli :: BoolDisj -> Bool)
            quickCheck  (mlr :: BoolDisj-> Bool)
        it "Combine should hold Monoid props" $ do
            let sa = combineSemigroupAssoc
            let mli = combineMonoidLeftIdentity
            let mlr = combineMonoidRightIdentity
            quickCheck  (sa)
            quickCheck  (mli :: String -> Combine String String -> Bool)
            quickCheck  (mlr :: String -> Combine String String -> Bool)
        it "Mem should hold Monoid props" $ do
            let sa = memSemigroupAssoc
            let mli = memMonoidLeftIdentity
            let mlr = memMonoidRightIdentity
            quickCheck  (sa)
            quickCheck  (mli)
            quickCheck  (mlr)