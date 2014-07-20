import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "scores function" $ do
        it "should process empty entries, yielding empty result" $ do
            scores [] `shouldBe` []

        it "should just show cards if a player has folded" $ do
            scores ["9h 5s", "7s Ts Ks Kd 9d"] `shouldBe` ["9h 5s", "7s Ts Ks Kd 9d"] 

        it "should rank the only High Card in a game as the winner" $ do
            scores ["Kc Qd Jc 8h 7h 5h 2s","Kc 2s"] `shouldBe`  ["Kc Qd Jc 8h 7h 5h 2s High Card (winner)","Kc 2s"]
