module Main where
import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
genList :: Int -> [(String, Int)]
genList n = go n []
    where go 0 xs = ("0", 0) : xs
          go n' xs = go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

testMap' :: M.Map String Int
testMap' = M.fromList$ map (\(k,v)->("-"++k, negate v) )pairList

testSet :: S.Set String 
testSet = S.fromList$ map fst pairList

testSet' :: S.Set String 
testSet' = S.fromList$ map (("-" ++).fst) pairList

singleList :: [Int]
singleList = [0..9001]

testVectorBoxed :: VB.Vector Int
testVectorBoxed = VB.fromList singleList

testVectorUnboxed :: VU.Vector Int
testVectorUnboxed = VU.fromList singleList

mainMap :: IO ()
mainMap = defaultMain
    [ bench "lookup one thing, list" $
    whnf (lookup "doesntExist") pairList
    , bench "lookup one thing, map" $
    whnf (M.lookup "doesntExist") testMap
    ]


mainSetCompMap :: IO ()
mainSetCompMap = defaultMain
    [ bench "lookup one thing, map" $
    whnf (M.lookup "doesntExist") testMap
    , bench "lookup one thing, set" $
    whnf (S.member "doesntExist") testSet
    , bench "insert one thing, map" $
    whnf (M.insert "doesntExist" (-1)) testMap
    , bench "insert one thing, set" $
    whnf (S.insert "doesntExist") testSet
    , bench "union Map" $
    whnf (M.union testMap') testMap
    , bench "union Set" $
    whnf (S.union testSet') testSet
    ]


vectorMemoryProfling :: IO ()
vectorMemoryProfling  = defaultMain
    [ bench "Generating Boxed Vector" $ 
    whnf (VB.fromList) singleList
    , bench "Generating Unboced Vector" $
    whnf (VU.fromList) singleList
    ]
main = mainSetCompMap