import Test.Hspec
import PokerHand

main :: IO ()
main = hspec $ do
    describe "scores function" $ do
        it "should process empty entries, yielding empty result" $ do
            scoreLines [] `shouldBe` []

        it "should just show cards if a player has folded" $ do
            scoreLines ["9h 5s", "7s Ts Ks Kd 9d"] `shouldBe` ["9h 5s", "7s Ts Ks Kd 9d"] 

        it "should rank the only High Card in a game as the winner" $ do
            scoreLines ["Kc Qd Jc 8h 7h 5h 2s","Kc 2s"] `shouldBe`  ["Kc Qd Jc 8h 7h 5h 2s High Card (winner)","Kc 2s"]

    describe "Card values" $ do
        it "should be created from a string in the form [A|K|Q|J|T|9..2][h|s|c|d]" $ do
            card "9h" `shouldBe` Card 9 Heart
            card "5s" `shouldBe` Card 5 Spade 
            card "3c" `shouldBe` Card 3 Clover
            card "2d" `shouldBe` Card 2 Diamond

        it "should be created with higher values for A,K,Q,J and T" $ do
            card "Ah" `shouldBe` Card 14 Heart
            card "Ks" `shouldBe` Card 13 Spade
            card "Qc" `shouldBe` Card 12 Clover
            card "Jd" `shouldBe` Card 11 Diamond
            card "Ts" `shouldBe` Card 10 Spade

        it "should compare one another" $ do
            compare (card "Ah") (card "Ks") `shouldBe` GT

        it "should compare values only, disregarding suit" $ do
            compare (card "Th") (card "Ts") `shouldBe` EQ

        it "should extract its value" $ do
            value (card "Qc") `shouldBe` 12

        it "should extract its suit" $ do
            suit (card "Qc") `shouldBe` Clover

    describe "hands function" $ do
        it "can extract all possible sequences of 5 items from a list" $ do
            hands "ABCDEF" `shouldBe` ["ABCDE","ABCDF","ABCEF","ABDEF","ACDEF","BCDEF"]

    describe "ranking function" $ do
        it "should rank High Cards hands with higher card" $ do
            ranking (map card ["3s","6c","Ts","Qh","5d"]) `shouldBe` HighCard (value (card "Qh"))
                    
    describe "bestHand function" $ do
        it "should find the best hand given a list of cards" $ do
            bestHand (map card ["3s","6c","Ts","Qh","5d","As","Kh"]) 
                `shouldBe` HighCard (value (card "As"))
