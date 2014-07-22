module Main
where
import PokerHand

main :: IO ()
main = interact (unlines . displayRound . lines)
    
