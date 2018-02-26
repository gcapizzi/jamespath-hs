import Test.Hspec

import JMESPath

main :: IO ()
main = hspec $
  describe "JMESPath.search" $ do
    context "with an identifier" $ do
      it "returns the value of the corresponding field in the input object" $ do
        search "a" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"foo\""
        search "b" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"bar\""
        search "c" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"baz\""

      context "when the field does not exist" $
        it "returns null" $
          search "x" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "null" 

      context "when the field is quoted" $
        it "works" $ do
          search "\"with space\"" "{\"with space\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"special chars: !@#\"" "{\"special chars: !@#\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"\\\"\\b\\f\\n\\r\\t\"" "{\"\\\"\\b\\f\\n\\r\\t\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"\\u2713\"" "{\"\\u2713\": \"value\"}" `shouldBe` Right "\"value\""

    context "with a sub-expression" $
      it "returns the corresponding value" $ do
        search "foo.bar" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "\"value\""
        search "foo.\"bar\"" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "\"value\""
        search "foo.baz" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "null"
        search "foo.bar.baz" "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}" `shouldBe` Right "\"value\""
