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

        it "should be a set of 52 distinct elements" $ do
            length (nub (cards deck)) `shouldBe` 52
                where deck =   "2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah"
                           ++ " 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks As"
                           ++ " 2d 3d 4d 5d 6d 7d 8d 9d Td Jd Qd Kd Ad"
                           ++ " 2c 3c 4c 5c 6c 7c 8c 9c Tc Jc Qc Kc Ac"
