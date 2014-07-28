module Main
where

computeScores :: String -> String
computeScores = unlines . mystery . lines

main :: IO()
main = interact computeScores 
