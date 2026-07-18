module Main where

import Cards.Logic
import Cards.Types
import Cards.Monads

import Games.Poker.Types
import Games.Poker.Logic

import Data.Maybe (mapMaybe)
import Control.Monad.State
import System.Random (newStdGen)


main :: IO ()
main = do
    let fresh = freshDeck

    systemGen <- newStdGen
    let cardState = CardState { stateGen = systemGen, stateDeck = fresh, stateDiscard = [] }
        drawAction :: CardMonad [Card]
        drawAction = do
            shuffleDeck
            draw 5
    
    let (hand1,newState) = runState drawAction cardState
        result1 = translateHand hand1
        hand2 = evalState drawAction newState
        result2 = translateHand hand2
    
    putStrLn "Hand 1:"
    putStrLn $ show hand1
    putStrLn $ show result1
    putStrLn "========================"
    putStrLn "Hand 2:"
    putStrLn $ show hand2
    putStrLn $ show result2
    putStrLn "========================"
    putStrLn "Who wins?"
    if result1 > result2 then putStrLn "Hand 1 wins"
    else putStrLn "Hand 2 wins"


getShuffledDeck :: IO Deck
getShuffledDeck = do
    let fresh = freshDeck

    systemGen <- newStdGen
    let cardState = CardState { stateGen = systemGen, stateDeck = fresh }
        shuffleAction = shuffleDeck
        (_,newState) = runState shuffleAction cardState
        newDeck = stateDeck newState
    return newDeck

testSpecificHands :: IO ()
testSpecificHands = do
    let flushHand = mapMaybe easyConvert ["AS","KS","QS","JS","TS"]
        fhHand    = mapMaybe easyConvert ["3S","3C","3D","2D","2H"]
        hand   = flushHand
        result = translateHand hand
    putStrLn $ show hand
    putStrLn $ show result

easyConvert :: String -> Maybe Card
easyConvert [val,st] = do
        goodVal  <- getVal val
        goodSuit <- getSuit st
        return $ Card { value = goodVal, suit = goodSuit}
    where
        getVal :: Char -> Maybe Value
        getVal v = case v of
            'J' -> return Jack
            'Q' -> return Queen
            'K' -> return King
            'A' -> return Ace
            'T' -> return Ten
            n   -> return (toEnum ((read [n] :: Int) - 2) :: Value)
        getSuit :: Char -> Maybe Suit
        getSuit s = case s of
            'S' -> return Spades
            'H' -> return Hearts
            'C' -> return Clubs
            'D' -> return Diamonds
            _ -> Nothing
easyConvert _ = Nothing