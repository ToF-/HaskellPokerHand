module PokerHand
where

data Value = King | Ace
    deriving (Show, Eq)

data Suit  = Heart | Spade | Diamond | Clover
    deriving (Show, Eq)

data Card  = Card Value Suit
    deriving (Show, Eq)

card :: String -> Card
card ['K',s] = Card King (suitFromString s)
card "Ah" = Card Ace Heart
    
suitFromString :: Char -> Suit
suitFromString 'h' = Heart
suitFromString 's' = Spade
suitFromString 'd' = Diamond
suitFromString 'c' = Clover



suit :: Card -> Suit
suit (Card _ s) = s
