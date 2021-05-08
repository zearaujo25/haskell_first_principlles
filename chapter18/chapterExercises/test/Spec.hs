import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec
import Test.Hspec.Checkers
import Monads

type SSI = (String, String, Int)

sumTrigger :: Sum String SSI
sumTrigger = undefined

nopeTrigger :: Nope SSI
nopeTrigger = undefined

identityTrigger :: Identity SSI
identityTrigger = undefined

listTrigger :: List SSI
listTrigger = undefined

main :: IO ()
main = hspec $ do
    describe "Sum Monad " $ do
        testBatch  (monad sumTrigger)
    describe "Nope Monad " $ do
        testBatch  (monad sumTrigger)
    describe "Identity Monad " $ do
        testBatch  (monad identityTrigger)
    describe "List Monad " $ do
        testBatch  (monad listTrigger)