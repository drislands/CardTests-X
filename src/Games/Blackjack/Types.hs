module Games.Blackjack.Types where

import Cards.Types

data Hand = Bust | Blackjack | Hand Int
    deriving (Show,Eq)

type Money = Int

data Player = Player
    { playerId   :: Int
    , playerHand :: [Card]
    , wallet     :: Money
    , wager      :: Money
    }