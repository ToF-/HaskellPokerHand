module PokerHand
where

data Value = King | Ace
    deriving (Show, Eq)

data Suit  = Heart | Spade
    deriving (Show, Eq)

data Card  = Card Value Suit
    deriving (Show, Eq)

card :: String -> Card
card "Ks" = Card King Spade
card "Ah" = Card Ace Heart
