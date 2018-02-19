import Test.Hspec

import JMESPath

main :: IO ()
main = hspec $
  describe "JMESPath.search" $
    context "with an identifier" $
      it "returns the value of the corresponding field in the input object" $ do
        search "a" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` "\"foo\""
        search "b" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` "\"bar\""
        search "c" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` "\"baz\""
