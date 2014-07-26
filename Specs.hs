import Test.Hspec
import PokerHand
import Data.List

main :: IO ()
main = hspec $ do
    describe "Cards" $ do
        it "should be extracted from Strings" $ do
            card "Ks" `shouldBe` Card King Spades
            card "Ah" `shouldBe` Card Ace Hearts

        it "should have one of four distinct suits" $ do
            length (nub (map suit (cards "Kh Ks Kd Kc")))
                `shouldBe` 4

        it "should have one of four distinct ranks" $do
            let deck = "2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah"
            length (nub (cards deck)) `shouldBe` 13

        it "should be stricly ordered within a same suit" $ do
            let set = map rank $ cards "2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah"
                isOrdered [x] = True
                isOrdered (x:y:xs) = x < y && isOrdered (y:xs)
            isOrdered set `shouldBe` True

    describe "the bestHand function" $ do
        it "should detect that a player folded when the list contains less than 7 cards" $ do
            bestHand (cards "4h Td 3c Ks Qd 8s") `shouldBe` Fold 

        it "should detect the best High Card when the list contains a High Card" $ do
            bestHand (cards "4h Td 3c Ks Qd 8s 6s") `shouldBe` HighCard [King, Queen, Ten, Eight, Six]

        it "should detect the best Fush when the list contains a flush" $ do
            bestHand (cards "4h Th 3d Kh Qh 8d 6h") `shouldBe` Flush [King, Queen, Ten, Six, Four]

    describe "the scores function" $ do
        it "should compute hands from a list of sets of cards and tag the winner" $ do
            let player1 = cards "4h Td 3c Ks Qd 8s"
                player2 = cards "4h Td 3c Ks Qd 8s 6s"
            scores [player1, player2] `shouldBe` [(Fold,False),(HighCard [King, Queen, Ten, Eight, Six], True)]
        
        it "should tag several winners for several best ranking of hands" $ do
            let player1 = cards "4h Td 3c Ks Qd 8s 6s"
                player2 = cards "4h Td 3c Ks Qd 8s 6s"
            map snd (scores [player1, player2]) `shouldBe` [True, True]

        it "should tag no winner when all players fold" $ do
            let player1 = cards "4h Td 3c Ks Qd 8s"
                player2 = cards "4h Td 3c Ks Qd 8s"
            map snd (scores [player1, player2]) `shouldBe` [False, False]

    describe "the displayScores function" $ do
        it "should display the scores for a given input" $ do
            let input  = "4h Td 3c Ks Qd 8s\n4h Td 3c Ks Qd 8s 6s\n 4h Td 3c Qs Js 7h 2s\n"
                output = "4h Td 3c Ks Qd 8s\n4h Td 3c Ks Qd 8s 6s High Card (winner)\n 4h Td 3c Qs Js 7h 2s High Card\n"
            displayScores input `shouldBe` output

