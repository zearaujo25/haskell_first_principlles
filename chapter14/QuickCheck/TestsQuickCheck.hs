module TestsQuickCheck where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

half x = x/2

halfIdentity = (*2) . half

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where   go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x >= y)


plusAssociative x y z =  x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

multAssociative x y z =  x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x


quoteProperty x y = (quot x y)*y + (rem x y) == x  
divProperty x y = (div x y)*y + (mod x y) == x  

powerAssossiative x y z =  x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative x y = x ^ y == y ^ x

reverseProperty x = (reverse . reverse$x)  == (id x)

foldContatPlusProperty x list= (foldr (:) [x] list) == (++) list [x]

foldContatFunctionProperty x = (foldr (++) [] x) == (concat x)

f n xs = length (take n xs) == n

fReadShow x = (read (show x)) == x

square x = x * x
squareIdentity = square . sqrt


twice f = f . f
fourTimes = twice . twice

capitalIdepotencyProperty x =
    (capitalizeWord x
    == twice capitalizeWord x)
    &&
    (capitalizeWord x
    == fourTimes capitalizeWord x)
    

capitalizeWord [] = []
capitalizeWord (h:t) = toUpper h : map toUpper t

sortProperty x =
    (sort x
    == twice sort x)
    &&
    (sort x
    == fourTimes sort x)


data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [return Fulse,return Frue]

genFreqFool :: Gen Fool
genFreqFool = frequency [(2,return Fulse),(1,return Frue)]

instance Arbitrary Fool where
arbitrary = genFool

main :: IO ()
main = hspec $ do 
    describe "TestsQuickCheck" $ do

        it "Half identity holds" $ do
            property $ \x -> (halfIdentity x) == (x :: Double)

        it "Ordered List holds" $ do
            property $ \x ->  (listOrdered.sort)  (x::[Int]) 

        it "Plus assossiative " $ do
            property $ \x -> \y-> \z->  plusAssociative  (x::Int) (y::Int) (z::Int) 

        it "Plus Commutative " $ do
            property $ \x -> \y->   plusCommutative  (x::Int) (y::Int)

        it "Mult assossiative " $ do
            property $ \x -> \y-> \z->  multAssociative  (x::Int) (y::Int) (z::Int) 

        it "Mult Commutative " $ do
            property $ \x -> \y->   multCommutative  (x::Int) (y::Int)

        it "Quote  Property " $ do
            property $ \x -> \y-> quoteProperty  (x::Int) (y::Int)

        it "Div  Property " $ do
            property $ \x -> \y-> quoteProperty  (x::Int) (y::Int)

        it "Power assossiative " $ do
            property $ \x -> \y-> \z->  powerAssossiative  (x::Int) (y::Int) (z::Int) 

        it "Power Commutative " $ do
            property $ \x -> \y->   powerCommutative  (x::Int) (y::Int)

        it "Reverse id " $ do
            property $ \x -> reverseProperty   (x::[String]) 

        it "Fold concat " $ do
            property $ \x -> \y-> foldContatPlusProperty (x::String) (y::[String]) 

        it "Fold concat plus" $ do
            property $ \x -> foldContatFunctionProperty (x::[String]) 

        it "Take" $ do
            property $ \x -> \y  -> f (x::Int) (y::[String])

        it "Read Show" $ do
            property $ \x ->  fReadShow (x::Int) 

        it "Square" $ do
            property $ \x ->  (squareIdentity x) == (x::Float) 

        it "Capitalize property idempotency" $ do
            property $ \x ->  capitalIdepotencyProperty  (x::String)

        it "Capitalize property idempotency" $ do
            property $ \x ->  capitalIdepotencyProperty  (x::String)

        it "Sort property idempotency" $ do
            property $ \x ->  sortProperty  (x::String)

        it "Fool test" $ do
            property $ \x ->  (True) || (==) Fulse  (x::Fool)



