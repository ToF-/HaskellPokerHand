module PokerHand
where

type Card = String
data Rank = HighCard

scores :: [String] -> [String]
scores = map score 

score :: String -> String        
score hs = 
    let
        cs = cards hs
        sc = case bestRank cs of 
                Just r -> " " ++ show r ++ " (winner)"
                Nothing -> ""
    in hs ++ sc
        
cards :: String -> [Card]
cards = words

instance Show Rank
    where show HighCard = "High Card"

bestRank :: [Card] -> Maybe Rank
bestRank cs = case length cs of
                7 -> Just HighCard
                _ -> Nothing
