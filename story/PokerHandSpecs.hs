import Test.Hspec

main :: IO()
main = hspec $ do
    describe "the appendScore function" $ do
        it "should append the display of scores to a list of lines" $ do
            let lines = ["A"
                        ,"B"]
                scores = [(Flush,True)
                         ,(Pair, False)]
            appendScores lines scores `shouldBe` 
                         ["A Flush (winner)"
                         ,"B Pair"          ]
