module PokerHand
where
import Data.List (sortBy, subsequences)
import Data.Ord (comparing)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Suit  = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq)

data Card  = Card { rank::Rank, suit::Suit }
    deriving (Show, Eq)

card :: String -> Card
card [v,s] = Card (rankFromChar v) (suitFromChar s)

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

cards :: String -> [Card]
cards = (map card) . words
    
suitFromChar :: Char -> Suit
suitFromChar 'h' = Hearts
suitFromChar 's' = Spades
suitFromChar 'd' = Diamonds
suitFromChar 'c' = Clubs

data Hand = Fold
          | HighCard [Rank]
          | Flush [Rank]
    deriving (Eq, Ord)

instance Show Hand
    where show Fold = "Fold"
          show (HighCard _) = "High Card"
          show (Flush _)    = "Flush"

bestHand :: [Card] -> Hand
bestHand cs | length cs < 7 = Fold
            | otherwise     = best cs
    where 
        best = maximum . 
               map ranking .
               allHands .
               reverse . sortBy (comparing rank)
        
        allHands = filter ((==5).length) . subsequences 
        
        ranking cs | isFlush cs = Flush (map rank cs)
                   | otherwise  = HighCard (map rank cs)
 
        isFlush (c:cs) = all (\x -> suit x == suit c) cs

type Score = (Hand, Bool)

scores :: [[Card]] -> [Score]
scores ps = map score hands
    where
        score h = (h, h == best && best /= Fold)
        hands = map bestHand ps
        best  = maximum hands

displayScores :: String -> String
displayScores s =
    let
        ls = lines s
        hs = map cards ls
        ss = scores hs
        showHand Fold = ""
        showHand h    = " " ++ show h
        showWinner False = ""
        showWinner True  = " (winner)" 
        display s (h, w) = s ++ showHand h ++ showWinner w
    in unlines $ zipWith display ls ss
