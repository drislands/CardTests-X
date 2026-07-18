module Games.Poker.Types where

import Cards.Types

data Hand = HighCard [Value]          -- All five cards
    | Pair Value [Value]        -- Pair, then remaining Cards
    | TwoPair Value Value Value -- High Pair, low Pair, 5th Card
    | ThreeOfAKind Value        -- The value you have 3 of
    | Straight Value            -- High Card
    | Flush [Value]             -- All five cards
    | FullHouse Value Value     -- Triplet, then Pair
    | FourOfAKind Value         -- The value you have 4 of
    | StraightFlush Value       -- High Card
    | RoyalFlush          -- All Royal Flushes tie.
    deriving (Eq, Ord, Show)

