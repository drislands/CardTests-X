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
    let cardState = CardState { stateGen = systemGen, stateDeck = fresh }
        drawAction :: CardMonad [Card]
        drawAction = do
            shuffleDeck
            draw 5
    
    let hand = evalState drawAction cardState
        result = translateHand hand
    
    putStrLn $ show hand
    putStrLn $ show result


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
easyConvert [] = Nothing
easyConvert (_:[]) = Nothing
easyConvert (_:_:_:_) = Nothing
easyConvert (val:st:[]) = do
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