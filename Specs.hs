import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "scores function\n" $ do
        it "process empty entries, yielding empty result" $ do
            scores [] `shouldBe` []
