module Cards.Types where

import Data.Ord (comparing)


data Suit = Spades | Hearts | Clubs | Diamonds 
    deriving (Show, Eq, Enum, Bounded,Ord)

data Value = Ace | Two | Three | Four | Five | Six | Seven 
    | Eight | Nine | Ten | Jack | Queen | King 
    deriving (Show, Eq, Enum, Bounded,Ord)

data Card = Card
    { suit  :: Suit
    , value :: Value
    } deriving (Eq)

instance Ord Card where
    compare = comparing suit <> comparing value

instance Show Card where
    show (Card {suit = s,value = v}) = (show v) ++ " of " ++ (show s)

type Deck = [Card]