module Main where
import Test.Hspec
import Test.QuickCheck
import CeasarCipher

main :: IO ()
main = hspec $ do
    let testKey = 543
    describe "Reversable property" $ do
        it "Cipher should be reversable property" $ do
            property $ \x -> (((flip ceasarDecipher) testKey).((flip ceasarCipher) testKey)$x) == (x :: String)