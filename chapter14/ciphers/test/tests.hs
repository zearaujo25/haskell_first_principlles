module Main where
import Test.Hspec
import Test.QuickCheck
import CeasarCipher

main :: IO ()
main = hspec $ do
    describe "fillInCharacter" $ do
        it "error guess should be in error" $ do
            property $ \x -> x + 1 > (x :: Int)