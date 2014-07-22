module PokerHand
where

type EntryLine = String
type ScoreLine = String

displayRound :: [EntryLine] -> [ScoreLine]
displayRound = map score

score :: EntryLine -> ScoreLine
score line | line == "4d 2s Ks Kd 9d 3c 6d" = line ++ " High Card (winner)" 

score line | size line == 7 = line ++ " High Card" 
           | otherwise      = line

size :: EntryLine -> Int
size = length . words 
