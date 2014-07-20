module PokerHand
where
import Data.Char

data Suit = Spade | Heart | Clover | Diamond
    deriving (Show, Eq)
data Card = Card Int Suit
    deriving (Show, Eq)

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

type Rank = Integer
data Hand = HighCard Rank

scores :: [String] -> [String]
scores = map score 

score :: String -> String        
score s = 
    let
        cs = map card (words s)
        sc = showHandScore (bestHand cs)
    in s ++ if (not (null sc)) then " "++ sc ++ " (winner)" else ""
        
instance Show Hand
    where show (HighCard _) = "High Card"

bestHand :: [Card] -> Maybe Hand
bestHand cs = case length cs of
                7 -> Just (HighCard 14)
                _ -> Nothing

showHandScore :: Maybe Hand -> String
showHandScore (Just hand) = show hand
showHandScore Nothing     = ""
