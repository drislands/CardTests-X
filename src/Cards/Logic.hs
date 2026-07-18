module Cards.Logic where

import Cards.Types

freshDeck :: Deck
freshDeck =
    let suits  = [minBound .. maxBound] :: [Suit]
    in reverse $ foldl fillSuits [] suits
    where
        fillSuits :: [Card] -> Suit -> [Card]
        fillSuits cards newSuit =
            let values = [minBound .. maxBound] :: [Value]
            in foldl (\c v -> Card { suit = newSuit, value = v } : c ) cards values

shuffleDeck' :: [Int] -> Deck -> Deck
shuffleDeck' [] xs = xs
shuffleDeck' _ [] = []
shuffleDeck' (i:is) xs =
    let (front,back) = splitAt (i `mod` length xs) xs
    in  head back : shuffleDeck' is (front ++ tail back)