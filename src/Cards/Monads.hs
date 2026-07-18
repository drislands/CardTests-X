module Cards.Monads where

import Cards.Types
import Cards.Logic
import Control.Monad.State

import System.Random (StdGen, randomR)


data CardState  = CardState
    { stateGen     :: StdGen
    , stateDeck    :: Deck
    , stateDiscard :: Deck
    } deriving (Show)

type CardMonad a = State CardState a


shuffleInDiscard :: CardMonad ()
shuffleInDiscard = do
    s <- get
    let oldDeck    = stateDeck s
        oldDiscard = stateDiscard s
        newDeck = oldDeck ++ oldDiscard
    put s { stateDeck = newDeck, stateDiscard = [] }

discard :: [Card] -> CardMonad ()
discard cards = do
    s <- get
    let oldDiscard = stateDiscard s
        newDiscard = cards ++ oldDiscard
    put s { stateDiscard = newDiscard }

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

drawOne :: CardMonad (Maybe Card)
drawOne = do
    s <- get
    case stateDeck s of
        [] -> return Nothing
        (result:rest) -> do
            put s { stateDeck = rest }
            return $ Just result

draw :: Int -> CardMonad [Card]
draw n
    | n < 1     = return []
    | otherwise = do
        mCard <- drawOne
        case mCard of
            Just card -> do
                rest <- draw (n - 1)
                return (card : rest)
            Nothing -> do
                shuffleInDiscard

                -- Emergency fallback to prevent infinite loops
                stillEmpty <- gets (null . stateDeck)
                if stillEmpty
                    then return []
                    else draw n