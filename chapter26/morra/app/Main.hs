module Main where

import System.Random
import Control.Monad.State

data Game = Game {player::Int, computer::Int, maxTurns::Int} deriving (Show)

main :: IO ()
main = do 
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
    

