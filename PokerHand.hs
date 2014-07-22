module PokerHand
where

type EntryLine = String
type ScoreLine = String

displayRound :: [EntryLine] -> [ScoreLine]
displayRound = map score

score :: EntryLine -> ScoreLine
score line | size line == 7 = line ++ " High Card (winner)" 
           | otherwise      = line

size :: EntryLine -> Int
size = length . words 
