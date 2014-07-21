module PokerHand
where
import Data.Char
import Data.List

type Value = Int

data Suit = Spade | Heart | Clover | Diamond
    deriving (Show, Eq)

instance Ord Suit 
    where compare _ _ = EQ

data Card = Card Value Suit
    deriving (Show, Eq, Ord)

data Hand = NotAHand
          | HighCard Value
    deriving (Eq, Ord)

instance Show Hand
    where show NotAHand = ""
          show (HighCard _) = "High Card"

type Entry = String

type Line = String

type Score = (Hand, Bool)

card :: String -> Card
card [r,s] = Card (toValue r) (toSuit s)
    where
    toValue :: Char -> Int
    toValue 'A' = 14
    toValue 'K' = 13
    toValue 'Q' = 12
    toValue 'J' = 11
    toValue 'T' = 10
    toValue c = (ord c) - (ord '0')

    toSuit :: Char -> Suit
    toSuit 'h' = Heart
    toSuit 's' = Spade
    toSuit 'c' = Clover
    toSuit 'd' = Diamond

value :: Card -> Int
value (Card v s) = v

suit :: Card -> Suit
suit (Card v s) = s

hands :: [a] -> [[a]]
hands = filter (\s -> length s == 5) . subsequences

bestHand :: [Card] -> Hand
bestHand cs | length cs < 7 = NotAHand
bestHand cs | otherwise = maximum (map ranking (hands cs))

ranking :: [Card] -> Hand
ranking cards = HighCard (value (head (reverse (sort cards))))

scores :: [Entry] -> [Score]
scores entries = zip hands (map wins hands)
    where
        hands = map findBestHand  entries

        findBestHand :: Entry -> Hand
        findBestHand = bestHand . map card . words

        wins :: Hand -> Bool
        wins NotAHand = False
        wins hand     = hand == maximum hands

scoreLines :: [Entry] -> [Line]
scoreLines entries = zipWith scoreLine entries (scores entries)

scoreLine :: Entry -> Score -> Line
scoreLine s (NotAHand,_) = s
scoreLine s (hand,True)  = s ++ " " ++ show hand ++ " (winner)"
scoreLine s (hand,False) = s ++ " " ++ show hand

