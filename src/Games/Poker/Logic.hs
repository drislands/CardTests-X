module Games.Poker.Logic where

import Cards.Types
import Games.Poker.Types
import Data.List
import Data.Ord (comparing, Down (Down))

-- Gotta be exactly 5. TODO: enforce this?
translateHand :: [Card] -> Hand
translateHand inCards =
    let (high, low)  = getHighLow inCards
        (count, val) = getMostValues inCards
    in if isFlush inCards
        then if isStraight inCards
            then if high == Ace && low == Ten
                then RoyalFlush
                else StraightFlush high
            else Flush (reverse . sortValues $ inCards)
        else case count of
            4 -> FourOfAKind val
            3 -> getFullOrTriple inCards val
            2 -> getPairOrTwo inCards val
            _ -> HighCard (reverse . sortValues $ inCards)
    where
        getFullOrTriple :: [Card] -> Value -> Hand
        getFullOrTriple cards val =
            let groups = groupByMost cards
            in if length groups > 2
                then ThreeOfAKind val
                else FullHouse val (head . head . tail $ groups)
        getPairOrTwo :: [Card] -> Value -> Hand
        getPairOrTwo cards val =
            let groups = groupByMost cards
            in if length groups > 3
                then Pair val (reverse . sortValues $ cards)
                else
                    let [v1:_,v2:_,v3:_] = groups
                        highV = max v1 v2
                        lowV  = min v1 v2
                    in TwoPair highV lowV v3

sortValues :: [Card] -> [Value]
sortValues cards = sort $ map value cards

groupByMost :: [Card] -> [[Value]]
groupByMost = sortBy (comparing (Data.Ord.Down . length)) . group . sortValues

getHighLow :: [Card] -> (Value,Value)
getHighLow cards =
    let sorted = sortValues cards
        [low, _, _, _, high] = sorted
    in (high,low)

getMostValues :: [Card] -> (Int,Value)
getMostValues cards =
    let found = head . groupByMost $ cards
    in (length found, head found)

isFlush :: [Card] -> Bool
isFlush cards =
    let firstSuit = suit . head $ cards
        maybeBad  = find (/= firstSuit) $ map suit cards
    in case maybeBad of
        Nothing -> True
        _ -> False

isStraight :: [Card] -> Bool
isStraight cards =
    let (high, low) = getHighLow cards
        sorted = sortValues cards
    in if high /= Ace || low == Ten
        then let idealStraight = sort $ foldl (\(b:bs) _-> succ b : b : bs) [low] $ tail cards
            in idealStraight == sorted
        else sorted == [Two,Three,Four,Five,Ace] -- Because the Ace sorts to the top