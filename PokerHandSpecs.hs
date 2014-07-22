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
