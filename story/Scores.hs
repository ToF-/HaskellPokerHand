module Scores
where

type Score = (Maybe Kind, Bool)

mystery :: [String] -> [String]
mystery = map showScore

