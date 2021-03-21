module Main where
import Test.Hspec
import Test.QuickCheck
import First'
import MonoidProps

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)