import Test.Hspec

main :: IO()
main = hspec $ do
    describe "test 1" $ do
        it "addition" $ do
            1 + 1 `shouldBe` 2