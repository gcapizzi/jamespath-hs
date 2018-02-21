import Test.Hspec

import JMESPath

main :: IO ()
main = hspec $
  describe "JMESPath.search" $
    context "with an identifier" $ do
      it "returns the value of the corresponding field in the input object" $ do
        search "a" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"foo\""
        search "b" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"bar\""
        search "c" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"baz\""

      context "when the field does not exist" $
        it "returns null" $
          search "x" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "null" 
