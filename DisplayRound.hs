module Main
where
import Data.Char

main :: IO ()
main = interact displayRound
    where displayRound = show . length . lines
