module Cards.Monads where

import Cards.Types
import Cards.Logic
import Control.Monad.State

import System.Random (StdGen, randomR)


data CardState  = CardState
    { stateGen  :: StdGen
    , stateDeck :: Deck
    } deriving (Show)

type CardMonad a = State CardState a



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

drawOne :: CardMonad Card
drawOne = do
    s <- get
    let deck = stateDeck s
        (result:rest) = deck
    put s { stateDeck = rest }
    return result

draw :: Int -> CardMonad [Card]
draw n
    | n < 1 = return []
    | otherwise = replicateM n drawOne