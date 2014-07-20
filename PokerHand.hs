module PokerHand
where
import Data.Char
import Data.List

data Suit = Spade | Heart | Clover | Diamond
    deriving (Show, Eq)

instance Ord Suit 
    where compare _ _ = EQ

type Value = Int

data Card = Card Value Suit
    deriving (Show, Eq, Ord)

data Hand = NotAHand
          | HighCard Value
    deriving (Eq, Ord)
instance Show Hand
    where show NotAHand = ""
          show (HighCard _) = " High Card"

type Score = (Hand, Bool)

card :: String -> Card
card [r,s] = Card (toValue r) (toSuit s)

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


scores :: [String] -> [Score]
scores entries = let
        hands = map computeHand entries
        computeHand entry = bestHand (map card (words entry))
        winner = maximum hands
    in zip hands (map (\h -> h == winner && h /= NotAHand) hands)

scoreLines :: [String] -> [String]
scoreLines entries = zipWith displayScore entries (scores entries)
    where displayScore entry (hand, isWinner) = 
            entry ++ (show hand) ++ if isWinner then " (winner)" else ""

bestHand :: [Card] -> Hand
bestHand cs | length cs < 7 = NotAHand
bestHand cs | otherwise = maximum (map ranking (hands cs))

hands :: [a] -> [[a]]
hands = filter (\s -> length s == 5) . subsequences

ranking :: [Card] -> Hand
ranking cards = HighCard (value (head (reverse (sort cards))))
