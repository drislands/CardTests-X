module Games.Blackjack.Types where

data Hand = Bust | Blackjack | Hand Int
    deriving (Show,Eq)