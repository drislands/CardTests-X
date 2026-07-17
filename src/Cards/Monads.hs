module Cards.Monads where

import Cards.Types
import Control.Monad.State

import System.Random (StdGen, randomR)


data CardState  = CardState
    { stateGen  :: StdGen
    , stateDeck :: Deck
    } deriving (Show)

type CardMonad a = State CardState a


shuffle' :: [Int] -> [a] -> [a]
shuffle' (i:is) xs = 
    let (front, back) = splitAt (i `mod` length xs) xs
    in  (head back) : shuffle' is (front ++ tail back)

shuffleDeck :: CardMonad ()
shuffleDeck = do
    s <- get
    let deck = stateDeck s
        len  = length deck
    randomOrder <- replicateM len (rollInt (0,len-1))
    let newDeck = shuffleDeck' randomOrder deck
    put s { stateDeck = newDeck }
    

rollInt :: (Int, Int) -> CardMonad Int
rollInt range = do
    s <- get
    let (val,nextGen) = randomR range (stateGen s)
    put s { stateGen = nextGen }
    return val

shuffleDeck' :: [Int] -> Deck -> Deck
shuffleDeck' [] xs = xs
shuffleDeck' _ [] = []
shuffleDeck' (i:is) xs =
    let (front,back) = splitAt (i `mod` length xs) xs
    in  (head back) : shuffleDeck' is (front ++ tail back)