module Score
where
import Card
import Ranking
import Data.List

type Score = (Kind, Bool)

scores :: [[Card]] -> [Score]
scores ps = map score rankings
    where
        score r  = (kind r, r == winner && kind r /= Fold) 
        rankings = map bestRanking ps
        winner   = maximum rankings

displayScores :: String -> String
displayScores = unlines . displayScores' . lines
    where 
        displayScores' l = zipWith display l (scores (map cards l))

        display s (h,w) = s `with` showRanking h `with` showWinner w

        s `with` "" = s
        s `with` t  = s ++ ' ':t

        showRanking Fold         = ""
        showRanking HighCard     = "High Card"
        showRanking Pair         = "Pair"
        showRanking ThreeOfAKind = "Three Of A Kind"
        showRanking Flush        = "Flush"

        showWinner True          = "(winner)"
        showWinner False         = ""
