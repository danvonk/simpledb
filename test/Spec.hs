import Parser
import Sql
import Test.Hspec
import Text.Parsec.Prim (parse)
import Text.Parsec.Error

main :: IO ()
main = hspec $ do
  describe "insert-query" $ do
    it "can parse an INSERT query" $ do
      parse insertTable "parse error" "INSERT INTO foo VALUES (2, 3, 4)"
        `shouldBe` Right (Insert "foo" (Insertion (Just [Right 2, Right 3, Right 4])))
  describe "select-query" $ do
    it "can parse a SELECT query" $ do
      parse select "parse error" "SELECT name, location FROM addresses"
        `shouldBe` Right (Query ["name", "location"] ["addresses"] Nothing)
