module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList =
  WordList [String]
  deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed errors) =
      (intersperse ' ' $  fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed ++ " Errors: " ++ errors

minWordLength :: Int
minWordLength = 5


maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)


gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in     l >= minWordLength
                && l <maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (minWordLength,maxWordLength )
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (const Nothing) w) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = elem c g


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s e) c =

    Puzzle word newFilledInSoFar (c : s) newErrors

    where zipper guessed wordChar guessChar =

            if wordChar == guessed
            then Just wordChar
            else guessChar

          newFilledInSoFar = zipWith (zipper c) word filledInSoFar
          newErrors = if newFilledInSoFar == filledInSoFar && (not (elem c s)) then (c:e) else e 

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
      putStrLn $ "Your guess was: " ++ [guess]
      case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
          (_, True) -> do
              putStrLn "You already guessed that\
                      \ character, pick \
                      \ something else!"
              return puzzle
          (True, _) -> do
              putStrLn "This character was in the\
                      \ word, filling in the word\
                      \ accordingly"
              return (fillInCharacter puzzle guess)
          (False, _) -> do
              putStrLn "This character wasn't in\
                      \ the word, try again."
              return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed e) =
      if (length e) > 7 then
            do  putStrLn "You lose!"
                putStrLn $ "The word was: " ++ wordToGuess
                exitSuccess
      else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do  putStrLn "You win!"
        exitSuccess
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must\
                      \ be a single character"