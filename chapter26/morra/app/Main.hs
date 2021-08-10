module Main where

import System.Random
import Control.Monad.State

data Game = Game {player::Int, computer::Int, maxTurns::Int} deriving (Show)

main :: IO ()
main = do answer <- getStdRandom (randomR (1,100)) -- think of a number
          putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
          guesses <- execStateT (guessSession answer) 0
          putStrLn $ "Success in " ++ (show guesses) ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer =
    do gs <- lift getLine    -- get guess from user
       let g = read gs       -- convert to number
       modify (+1)           -- increment number of guesses
       case compare g answer of
              LT -> do lift $ putStrLn "Too low"
                       guessSession answer
              GT -> do lift $ putStrLn "Too high"
                       guessSession answer
              EQ -> lift $ putStrLn "Got it!"



main' :: IO ()
main' = do 
    putStrLn "Initiating Game"
    let initialGame = Game 0 0 5
    gameResult <- execStateT game initialGame
    putStrLn$ show gameResult


-- computer is even 
game :: StateT Game IO ()
game = do
    lift$ putStrLn "Pick a random number"
    ps <- lift$ getLine
    let p = read ps ::Int
    c <- getStdRandom (randomR (0,5)) :: StateT Game IO Int
    case even (p+c) of 
        True -> do 
            lift$ putStrLn "Computer won"
            handleTurn increaseComputer
        False -> do 
            lift$ putStrLn "Player won"
            handleTurn increasePlayer

increaseComputer:: Game -> Game
increaseComputer (Game p c m) = Game p (c+1) m

increasePlayer:: Game -> Game
increasePlayer (Game p c m) = Game (p+1) c m

handleTurn :: (Game -> Game) -> StateT Game IO ()
handleTurn increaseF = do 
    modify (increaseF)
    (Game p c max) <- get
    case (p==max || c == max) of
        True -> lift $ putStrLn "End" 
        False -> game
    

