import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "simpledb-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
