module PokerHand
where

data Value = King   deriving (Show,Eq)
data Suit  = Spade  deriving (Show,Eq)
data Card  = Card Value Suit
    deriving (Show, Eq)

card :: String -> Card
card "Ks" = Card King Spade
