module Games.Poker.Types where

import Cards.Types

data Hand = RoyalFlush          --
    | StraightFlush Value       -- High Card
    | FourOfAKind Value         -- 
    | FullHouse Value Value     -- Triplet, then Pair
    | Flush [Value]             -- All five cards
    | Straight Value            -- High Card
    | ThreeOfAKind Value        --
    | TwoPair Value Value Value -- High Pair, low Pair, 5th Card
    | Pair Value [Value]        -- Pair, then remaining Cards
    | HighCard [Value]          -- All five cards
    deriving (Eq, Ord)

