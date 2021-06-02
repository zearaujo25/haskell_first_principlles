import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec
import Test.Hspec.Checkers
import ChapterExercises

type SSI = (String, String, [Int])

identityTrigger :: Identity SSI
identityTrigger = undefined

constantTrigger :: Constant String SSI
constantTrigger = undefined

optionalTrigger :: Optional SSI
optionalTrigger = undefined

listTrigger :: List SSI
listTrigger = undefined

threeTrigger :: Three String String SSI
threeTrigger = undefined

pairTrigger :: Three String String SSI
pairTrigger = undefined

bigTrigger :: Big String SSI
bigTrigger = undefined


biggerTrigger :: Bigger String SSI
biggerTrigger = undefined

main :: IO ()
main = hspec $ do
    describe "Identity Traversable " $ do
        testBatch  (traversable identityTrigger)
        testBatch  (applicative identityTrigger)
        testBatch  (functor identityTrigger)
    describe "Constant Traversable " $ do
        testBatch  (traversable constantTrigger)
        testBatch  (applicative constantTrigger)
        testBatch  (functor constantTrigger)
    describe "Optional Traversable " $ do
        testBatch  (traversable optionalTrigger)
        testBatch  (applicative optionalTrigger)
        testBatch  (functor optionalTrigger)
    describe "List Traversable " $ do
        testBatch  (traversable listTrigger)
        testBatch  (applicative listTrigger)
        testBatch  (functor listTrigger)
    describe "Three Traversable " $ do
        testBatch  (traversable threeTrigger)
        testBatch  (applicative threeTrigger)
        testBatch  (functor threeTrigger)
    describe "Pair Traversable " $ do
        testBatch  (traversable pairTrigger)
        testBatch  (applicative pairTrigger)
        testBatch  (functor pairTrigger)
    describe "Big Traversable " $ do
        testBatch  (traversable bigTrigger)
        testBatch  (applicative bigTrigger)
        testBatch  (functor bigTrigger)
    describe "Bigger Traversable " $ do
        testBatch  (traversable biggerTrigger)
        testBatch  (applicative biggerTrigger)
        testBatch  (functor biggerTrigger)