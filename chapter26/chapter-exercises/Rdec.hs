module Rdec where

import Control.Monad.Trans.Reader 
import Control.Monad.Identity
import Control.Monad.Trans.State 
import Control.Monad.Trans.Maybe
import Control.Monad

rDec :: Num a => Reader a a
rDec = reader$ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = reader show 


rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT$ \r-> do
    print$ "Hi: " ++ (show r)
    return$ r+1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT$ \s-> do
    print$ "Hi: " ++ (show s)
    return$ (show s,s+1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT$ do
    v <- getLine
    if (isValid v) then  return$ Just v else return$ Nothing

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e ->
            putStrLn
                ("Good, was very excite: " ++ e)