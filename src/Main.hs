module Main where

import Cards.Logic
import Cards.Types
import Cards.Monads

import Control.Monad.State
import System.Random (newStdGen)


main :: IO ()
main = do
    putStrLn "Making a fresh deck"
    let fresh = freshDeck

    systemGen <- newStdGen
    let cardState = CardState { stateGen = systemGen, stateDeck = fresh }
        shuffleAction = shuffleDeck
        (_,newState) = runState shuffleAction cardState
        newDeck = stateDeck newState

    putStrLn $ show newDeck