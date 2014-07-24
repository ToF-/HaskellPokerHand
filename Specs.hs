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
            length (nub (cards "Kh Ks Kd Kc"))
                `shouldBe` 4
