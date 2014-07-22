import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "displayRound function" $ do
        it "should show no winner on a round with only players who fold" $ do
            displayRound ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"            ] 
              `shouldBe` ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"            ]

        it "should show a winner on a round with only one player with a High Card" $ do
            displayRound ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"
                         ,"4d 2s Ks Kd 9d 3c 6d"]
              `shouldBe` ["Ac Qc Ks Kd 9d 3c"
                         ,"9h 5s"
                         ,"4d 2s Ks Kd 9d 3c 6d High Card (winner)"]

        it "should show a winner on a round with several High Card, with one ranking higher" $ do
            displayRound ["9h 5s"
                         ,"4d 2s Qs Kd 9d 3c 6d" 
                         ,"4d 2s Ks Kd 9d 3c 6d"]
              `shouldBe` ["9h 5s"
                         ,"4d 2s Qs Kd 9d 3c 6d High Card"
                         ,"4d 2s Ks Kd 9d 3c 6d High Card (winner)"]


