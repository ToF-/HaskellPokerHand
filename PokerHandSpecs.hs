import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "displayRound function" $ do
        it "should show no winner on a round with only players who fold" $ do
            displayRound "Ac Qc Ks Kd 9d 3c\n9h 5s\n" `shouldBe` "Ac Qc Ks Kd 9d 3c\n9h 5s\n"
