import Test.Hspec
import PokerHand
import Data.List

main :: IO ()
main = hspec $ do
    describe "Cards" $ do
        it "should be extracted from Strings" $ do
            card "Ks" `shouldBe` Card King Spade
            card "Ah" `shouldBe` Card Ace Heart

        it "should have a one of four distinct suits" $ do
            length (nub (map suit (cards "Kh Ks Kd Kc")))
                `shouldBe` 4

        it "should be a set of 52 distinct elements" $ do
            let deck = "2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah"
                   ++ " 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks As"
                   ++ " 2d 3d 4d 5d 6d 7d 8d 9d Td Jd Qd Kd Ad"
                   ++ " 2c 3c 4c 5c 6c 7c 8c 9c Tc Jc Qc Kc Ac"
            length (nub (cards deck)) `shouldBe` 52

        it "should be stricly ordered within a same suit" $ do
            let set = cards "2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah"
                isOrdered [x] = True
                isOrdered (x:y:xs) = x < y && isOrdered (y:xs)
            isOrdered set `shouldBe` True

        it "should compare same values on different suit as equal" $ do
            compare (card "Ts") (card "Th") `shouldBe` EQ

    describe "the hand function" $ do
        it "should detect that a player folded when the list contains less than 7 cards" $ do
            hand (cards "4h Td 3c Ks Qd 8s") `shouldBe` Fold 

        it "should detect the best High Card when the list contains a High Card" $ do
            hand (cards "4h Td 3c Ks Qd 8s 6s") `shouldBe` HighCard [King, Queen, Ten, Eight, Six]

    describe "the score function" $ do
        it "should compute hands from a list of sets of cards and tag the winner" $ do
            let player1 = cards "4h Td 3c Ks Qd 8s"
                player2 = cards "4h Td 3c Ks Qd 8s 6s"
            score [player1, player2] `shouldBe` [(Fold,False),(HighCard [King, Queen, Ten, Eight, Six], True)]

