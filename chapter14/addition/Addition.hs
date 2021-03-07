-- Addition.hs
module Addition where
    
import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n    d count
            | n < d = (count, n)
            | otherwise =  go (n - d) d (count + 1)

multSumRecursive :: (Integral a) => a -> a -> a
multSumRecursive _ 0  = 0
multSumRecursive f1 f2  = f1 + (multSumRecursive f1 (f2-1))

main :: IO ()
main = hspec $ do
        describe "Addition" $ do
            it "1 + 1 is greater than 1" $ do
                (1 + 1) > 1 `shouldBe` True
            it "2 + 2 is equal to 4" $ do
                2 + 2 `shouldBe` 4
            it "22 divided by 5 is\
            \ 4 remainder 2" $ do
                dividedBy 22 5 `shouldBe` (4, 2)
            it "20 times 5 is 100" $ do
                multSumRecursive 20 5 `shouldBe` 100
            it "x + 1 is always\
            \ greater than x" $ do
                property $ \x -> x + 1 > (x :: Int)