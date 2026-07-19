module Games.Blackjack.Logic where

import Cards.Types
import Games.Blackjack.Types


translateHand :: [Card] -> Hand
translateHand cards =
    let aceCount = length $ filter ((== Ace) . value) cards
        total    = sum $ map translateCard cards
    in parseTotal total aceCount
    where
        parseTotal :: Int -> Int -> Hand
        parseTotal n ac
            | n == 21   = Blackjack
            | n >  21   = if ac < 1 
                then Bust 
                else parseTotal (n - 10) (ac - 1)
            | otherwise = Hand n

translateCard :: Card -> Int
translateCard card =
    case value card of
        Ace   -> 11
        King  -> 10
        Queen -> 10
        Jack  -> 10
        other -> fromEnum other + 2