import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "Cards" $ do
        it "should be extracted from Strings" $ do
            card "Ks" `shouldBe` Card King Spade
            card "Ah" `shouldBe` Card Ace Heart

        it "should have a suit" $ do
            map (suit.card) (words "Kh Ks Kd Kc")
                `shouldBe` [Heart, Spade, Diamond, Clover]
