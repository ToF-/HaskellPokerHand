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
card [v,s] = Card (valueFromChar v) (suitFromChar s)

valueFromChar :: Char -> Value
valueFromChar '2' = Two
valueFromChar '3' = Three
valueFromChar '4' = Four
valueFromChar '5' = Five
valueFromChar '6' = Six
valueFromChar '7' = Seven
valueFromChar '8' = Eight
valueFromChar '9' = Nine
valueFromChar 'T' = Ten
valueFromChar 'J' = Jack
valueFromChar 'Q' = Queen
valueFromChar 'K' = King
valueFromChar 'A' = Ace

cards :: String -> [Card]
cards = (map card) . words
    
suitFromChar :: Char -> Suit
suitFromChar 'h' = Heart
suitFromChar 's' = Spade
suitFromChar 'd' = Diamond
suitFromChar 'c' = Clover



suit :: Card -> Suit
suit (Card _ s) = s
