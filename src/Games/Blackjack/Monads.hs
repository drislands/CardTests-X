module Games.Blackjack.Monads where

import Control.Monad.State
    ( runState, MonadState(put, get), State )
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class
import Data.List
import Cards.Monads
import Cards.Types
import Games.Blackjack.Types
import Games.Blackjack.Logic (translateHand)


data BlackjackState = BlackjackState
    { cardState    :: CardState
    , dealerHand   :: [Card]
    , statePlayers :: [Player]
    }

type BlackjackMonad a = State BlackjackState a


-- Lets us convert CardMonad stuff into BlackjackMonad stuff
-- This will be for card-specific actions, like drawing and 
-- shuffling and discarding.
embedCardAction :: CardMonad a -> BlackjackMonad a
embedCardAction cardAction = do
    bState <- get
    let cState = cardState bState
        (result, newCState) = runState cardAction cState
    put bState { cardState = newCState }
    return result

addPlayer :: Int -> Money -> BlackjackMonad ()
addPlayer pId startingCash = do
    s <- get
    let newPlayer = Player {
          playerId = pId
        , playerHand = []
        , wallet = startingCash
        , wager = 0
        }
        oldPlayers = statePlayers s
        newPlayers = newPlayer : oldPlayers
    put s { statePlayers = newPlayers }
    return ()

getPlayer :: Int -> BlackjackMonad (Maybe Player)
getPlayer pId = do
    s <- get
    let players = statePlayers s
        maybePlayer = find ((== pId) . playerId) players
    return maybePlayer

-- We don't do any validation at this stage. If the player
-- wasn't already in the list, it will be after this.
dealPlayerIn :: Player -> Money -> BlackjackMonad Player
dealPlayerIn player ante = do
    -- Do some money conversion
    let startingMoney = wallet player
        newMoney      = startingMoney - ante
    hand <- embedCardAction (draw 2)
    -- Define the updated player, and get the rest of the list
    let result = player { wallet = newMoney, wager = ante, playerHand = hand }
    updatePlayer result
    return result

updatePlayer :: Player -> BlackjackMonad ()
updatePlayer player = do
    s <- get
    let otherPlayers = filter ((/= playerId player) . playerId) (statePlayers s)
    put s { statePlayers = player : otherPlayers }

hit :: Int -> BlackjackMonad (Maybe Hand)
hit pId = runMaybeT $ do
    p <- MaybeT $ getPlayer pId
    [nextCard] <- lift $ embedCardAction (draw 1)
    let newHand = nextCard : playerHand p
        result = translateHand newHand
        newPlayer = p { playerHand = newHand }
    lift $ updatePlayer newPlayer
    return result

-- Plays a hand until it's either a bust, blackjack, or a 
-- high enough value to stop.
autoPlayHand :: [Card] -> Int -> BlackjackMonad Hand
autoPlayHand cards hardstop
    | Bust      <- status = return status
    | Blackjack <- status = return status
    | Hand v    <- status, v >= hardstop = return status
    | otherwise = do
        nextCards <- embedCardAction (draw 1)
        case nextCards of
            (nextCard:_) -> autoPlayHand (nextCard : cards) hardstop
            _ -> return status
    where
        status = translateHand cards

playDealer :: BlackjackMonad Hand
playDealer = do
    s <- get
    let hardStop = 17
        dHand = dealerHand s
    autoPlayHand dHand hardStop
