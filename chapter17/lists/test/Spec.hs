import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec
import Test.Hspec.Checkers
import ZipList
import ValidationApplicative

type SSI = (String, String, Int)

trigger :: List SSI
trigger = undefined

trigger' :: ZipList' SSI
trigger' = undefined

validationTrigger :: Validation String SSI
validationTrigger = undefined

main :: IO ()
main = hspec $ do
    describe "List Applicative " $ do
        testBatch  (applicative trigger)
    describe "ZipList Applicative " $ do
        testBatch  (applicative trigger')
    describe "Validation Applicative " $ do
        testBatch  (applicative validationTrigger)
