module Main where

import Data.Char (toLower)
import Hangman (runGame,randomWord',freshPuzzle)

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle