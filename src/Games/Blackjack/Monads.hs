module Games.Blackjack.Monads where

import Control.Monad.State
    ( runState, MonadState(put, get), State )
import Data.List
import Cards.Monads
import Cards.Types
import Games.Blackjack.Types


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
    s <- get
    -- Do some money conversion
    let startingMoney = wallet player
        newMoney      = startingMoney - ante
    hand <- embedCardAction (draw 2)
    -- Define the updated player, and get the rest of the list
    let result = player { wallet = newMoney, wager = ante, playerHand = hand }
        otherPlayers  = filter ((/= playerId player) . playerId) (statePlayers s)
    put s { statePlayers = result : otherPlayers }

    return result