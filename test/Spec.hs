import Test.Hspec

import Cilia.CI.InternalRepo(BuildState(Passed))

main :: IO()
main = hspec $ do
    describe "test 1" $ do
        it "addition" $ do
            1 + 1 `shouldBe` 2

    describe "BuildState Show instance" $ do
        it "show Passed" $ do
            (show Passed) `shouldBe` "Passed"