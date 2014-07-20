import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "scores function\n" $ do
        it "should process empty entries, yielding empty result" $ do
            scores [] `shouldBe` []

        it "should just show cards if a player has folded" $ do
            scores ["9h 5s", "7s Ts Ks Kd 9d"] `shouldBe` ["9h 5s", "7s Ts Ks Kd 9d"] 
