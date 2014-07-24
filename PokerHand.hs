module PokerHand
where
import Data.List

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine
           | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Suit  = Heart | Spade | Diamond | Clover
    deriving (Show, Eq)
instance Ord Suit
    where compare _ _ = EQ

data Card  = Card Value Suit
    deriving (Show, Eq, Ord)

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

value :: Card -> Value
value (Card v s) = v

data Hand = Fold
          | HighCard [Value]
          | Flush [Value]
    deriving (Eq, Ord)
instance Show Hand
    where show Fold = ""
          show (HighCard _) = "High Card"
          show (Flush _)    = "Flush"

hand :: [Card] -> Hand
hand cs | length cs < 7 = Fold
        | otherwise = maximum (map rank (groupsOf5 (reverse (sort cs))))

groupsOf5 :: [Card] -> [[Card]]
groupsOf5 = filter (\cs -> length cs == 5) . subsequences

rank :: [Card] -> Hand 
rank cs | isFlush cs = Flush (map value cs)
        | otherwise  = HighCard (map value cs)
 
isFlush :: [Card] -> Bool
isFlush (c:cs) = all (\x -> suit x == suit c) cs

type Score = (Hand, Bool)

score :: [[Card]] -> [Score]
score ps = let
    hands = map hand ps
    best  = maximum hands
    score h = (h, h == best && best /= Fold)
    in map score hands

displayScore :: String -> String
displayScore s =
    let
        ls = lines s
        hs = map cards ls
        ss = score hs
        display s (h, w) = s ++ " " ++ show h ++ if w then " (winner)" else ""
    in unlines $ zipWith display ls ss
