module PokerHand
where

scores :: [String] -> [String]
scores = map score 
    where 
        score hs = case howManyCards hs of
                    7 -> hs ++ " High Card (winner)"
                    _ -> hs
        howManyCards = length . words 
