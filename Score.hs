module Score
where
import Card
import Ranking

type Score = (Ranking, Bool)

scores :: [[Card]] -> [Score]
scores ps = map score hands
    where
        score h = (h, h == best && best /= Fold)
        hands = map bestRanking ps
        best  = maximum hands

displayScores :: String -> String
displayScores = unlines . displayScores' . lines
    where 
        displayScores' l = zipWith display l (scores (map cards l))

        display s (h,w) = s ++ showRanking h ++ if w then " (winner)" else ""

        showRanking Fold = ""
        showRanking (HighCard _) = " " ++ "High Card"
        showRanking (Pair    _)  = " " ++ "Pair"
        showRanking (ThreeOfAKind _)  = " " ++ "Three Of A Kind"
        showRanking (Flush  _)   = " " ++ "Flush"
