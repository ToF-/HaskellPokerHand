module Card
where

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Suit  = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq)

data Card  = Card { rank::Rank, suit::Suit }
    deriving (Show, Eq)

card :: String -> Card
card [v,s] = Card (rankFromChar v) (suitFromChar s)
    where 
        rankFromChar :: Char -> Rank
        rankFromChar '2' = Two
        rankFromChar '3' = Three
        rankFromChar '4' = Four
        rankFromChar '5' = Five
        rankFromChar '6' = Six
        rankFromChar '7' = Seven
        rankFromChar '8' = Eight
        rankFromChar '9' = Nine
        rankFromChar 'T' = Ten
        rankFromChar 'J' = Jack
        rankFromChar 'Q' = Queen
        rankFromChar 'K' = King
        rankFromChar 'A' = Ace

        suitFromChar :: Char -> Suit
        suitFromChar 'h' = Hearts
        suitFromChar 's' = Spades
        suitFromChar 'd' = Diamonds
        suitFromChar 'c' = Clubs

cards :: String -> [Card]
cards = (map card) . words
    
