module Main where

import Test.Hspec
import Hangman 
import Data.Char (toLower)


getPuzzleErrors :: Puzzle -> [Char]
getPuzzleErrors (Puzzle _ _ _ e) = e

getPuzzleGuesses :: Puzzle -> [Char]
getPuzzleGuesses (Puzzle _ _ g _) = g

getPuzzleState :: Puzzle -> [Maybe Char]
getPuzzleState (Puzzle _ s _ _) = s


main :: IO ()
main = hspec $ do
    let testWord = "Test"
    let testPuzzle = freshPuzzle (fmap toLower testWord)

    describe "fillInCharacter" $ do
        it "error guess should be in error" $ do
            let wrongChar = 'a'
            let resultPuzzle = fillInCharacter testPuzzle wrongChar
            let resultErrors = getPuzzleErrors resultPuzzle
            (elem wrongChar resultErrors) `shouldBe` True

        it "error guess should be in guessed" $ do
            let wrongChar = 'a'
            let resultPuzzle = fillInCharacter testPuzzle wrongChar
            let resultGuessed = getPuzzleGuesses resultPuzzle
            (elem wrongChar resultGuessed) `shouldBe` True

        it "error should not change puzzle" $ do
            let wrongChar = 'a'
            let resultPuzzle = fillInCharacter testPuzzle wrongChar
            let testState = getPuzzleState testPuzzle
            let resulState = getPuzzleState resultPuzzle
            resulState `shouldBe` testState

    describe "handleGuess" $ do
        it "error guess should be in error" $ do
            let wrongChar = 'a'
            resultPuzzle <- handleGuess testPuzzle wrongChar
            let resultErrors = getPuzzleErrors resultPuzzle
            (elem wrongChar resultErrors) `shouldBe` True

        it "error guess should be in guessed" $ do
            let wrongChar = 'a'
            resultPuzzle <- handleGuess testPuzzle wrongChar
            let resultGuessed = getPuzzleGuesses resultPuzzle
            (elem wrongChar resultGuessed) `shouldBe` True

        it "error should not change puzzle" $ do
            let wrongChar = 'a'
            resultPuzzle <- handleGuess testPuzzle wrongChar
            let testState = getPuzzleState testPuzzle
            let resulState = getPuzzleState resultPuzzle
            resulState `shouldBe` testState