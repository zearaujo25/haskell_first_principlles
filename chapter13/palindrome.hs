import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower,isAlphaNum)
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let sentence = (map toLower).(filter isAlphaNum)$line1
    case (sentence == reverse sentence) of
        True -> putStrLn "It's a palindrome!"
        False -> do 
                putStrLn "Nope!"
                exitSuccess