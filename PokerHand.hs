module PokerHand
where

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine
           | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq)

data Suit  = Heart | Spade | Diamond | Clover
    deriving (Show, Eq)

data Card  = Card Value Suit
    deriving (Show, Eq)

card :: String -> Card
card [v,s] = Card (valueFromString v) (suitFromString s)

valueFromString :: Char -> Value
valueFromString '2' = Two
valueFromString '3' = Three
valueFromString '4' = Four
valueFromString '5' = Five
valueFromString '6' = Six
valueFromString '7' = Seven
valueFromString '8' = Eight
valueFromString '9' = Nine
valueFromString 'T' = Ten
valueFromString 'J' = Jack
valueFromString 'Q' = Queen
valueFromString 'K' = King
valueFromString 'A' = Ace

cards :: String -> [Card]
cards = (map card) . words
    
suitFromString :: Char -> Suit
suitFromString 'h' = Heart
suitFromString 's' = Spade
suitFromString 'd' = Diamond
suitFromString 'c' = Clover



suit :: Card -> Suit
suit (Card _ s) = s
