{-# LANGUAGE OverloadedStrings #-}
module Main where
 
import Prelude hiding (lookup)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
    Config {
    -- that's one, one click!
    -- two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty =
    ScottyT Text (ReaderT Config IO)
    
type Handler =
    ActionT Text (ReaderT Config IO)

getPrefix :: Handler Text
getPrefix = do
  config <- lift $ ask  
  return$ prefix config

getHits :: Handler (M.Map Text Integer)
getHits = do
  config <- lift $ ask 
  liftIO$ readIORef $ counts config

updateHits :: (M.Map Text Integer) -> Handler ()
updateHits newM = do
  config <- lift $ ask 
  let ioRefCounts =  counts config
  liftIO$ writeIORef ioRefCounts newM

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer) 
bumpBoomp k m = (newM, newV) where 
    v = maybe 0 id (M.lookup k m)
    newV = v+1
    newM = M.insert k newV m

getAndUpdateHits :: Text -> Handler (Integer)
getAndUpdateHits text = do 
    counts <- getHits
    let (newMap,newValue) = bumpBoomp text counts 
    updateHits newMap
    return newValue

app :: Scotty ()
app =get "/:key" $ do
    unprefixed <- param "key"
    prefix <- getPrefix
    let key' = mappend prefix unprefixed
    counts <- getHits 

    newInteger <- getAndUpdateHits key'

    html $mconcat [ "<h1>Success! Count was: "
                    , TL.pack $ show newInteger
                    , "</h1>"
                    ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let convertedText = TL.pack(prefixArg)
        config = Config counter convertedText
        runR r = runReaderT r config
    scottyT 3000 runR app