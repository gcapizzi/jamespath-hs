import Test.Hspec

import Data.Either

import JMESPath

main :: IO ()
main = hspec $
  describe "JMESPath.search" $ do
    context "with an identifier" $ do
      it "returns the value of the corresponding field in the input object" $ do
        search "a" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "\"foo\""
        search "B" "{\"A\": \"foo\", \"B\": \"bar\", \"C\": \"baz\"}" `shouldBe` Right "\"bar\""
        search "_3" "{\"_1\": \"foo\", \"_2\": \"bar\", \"_3\": \"baz\"}" `shouldBe` Right "\"baz\""

      context "when the field does not exist" $
        it "returns null" $
          search "x" "{\"a\": \"foo\", \"b\": \"bar\", \"c\": \"baz\"}" `shouldBe` Right "null" 

      context "when the identifier starts with a number" $
        it "fails" $
          search "1_a" "{\"1a\": \"foo\"}" `shouldSatisfy` isLeft

      context "when the identifier is quoted" $ do
        it "works" $ do
          search "\"with space\"" "{\"with space\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"special chars: !@#\"" "{\"special chars: !@#\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"\\\"\\b\\f\\n\\r\\t\"" "{\"\\\"\\b\\f\\n\\r\\t\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"\\u2713\"" "{\"\\u2713\": \"value\"}" `shouldBe` Right "\"value\""
          search "\"\\t4\\u2713\\ud9da\\udd15\"" "{\"\\t4\\u2713\\ud9da\\udd15\": true}" `shouldBe` Right "true"
          search "\"\x103C02\"" "{\"\\udbcf\\udc02\": true}" `shouldBe` Right "true"

        context "when the identifier is an empty string" $
          it "fails" $
            search "\"\"" "{\"\": \"value\"}" `shouldSatisfy` isLeft

    context "with a sub-expression" $ do
      it "returns the corresponding value" $ do
        search "foo.bar" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "\"value\""
        search "foo.\"bar\"" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "\"value\""
        search "foo.bar.baz" "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}" `shouldBe` Right "\"value\""
        search "foo\n.\nbar\n.baz" "{\"foo\": {\"bar\": {\"baz\": \"value\"}}}" `shouldBe` Right "\"value\""

      context "when the key does not exist" $
        it "returns null" $
          search "foo.baz" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "null"

      context "when the expression does not return an object" $
        it "returns null" $
          search "foo.bar.baz" "{\"foo\": {\"bar\": \"value\"}}" `shouldBe` Right "null"

    context "with an index expression" $ do
      it "returns the nth value of a list" $ do
        search "[0]" "[\"foo\", \"bar\", \"baz\"]" `shouldBe` Right "\"foo\""
        search "[ 1 ]" "[\"foo\", \"bar\", \"baz\"]" `shouldBe` Right "\"bar\""
        search "[3]" "[\"foo\", \"bar\", \"baz\"]" `shouldBe` Right "null"

      context "when the index is negative" $
        it "returns the nth to last value of a list" $ do
          search "[-1]" "[\"foo\", \"bar\", \"baz\"]" `shouldBe` Right "\"baz\""
          search "[-4]" "[\"foo\", \"bar\", \"baz\"]" `shouldBe` Right "null"

    context "with sub-expressions and index expressions" $
      it "works" $
        search "foo[0].bar.baz[1]" "{\"foo\": [{\"bar\": {\"baz\": [false, \"value\"]}}]}" `shouldBe` Right "\"value\""

    context "with an array projection" $
      it "applies the following expressions to each element of a list" $ do
        search "[*]" "[{\"bar\": 1}, {\"bar\": 2}, {\"bar\": 3}]" `shouldBe` Right "[{\"bar\":1},{\"bar\":2},{\"bar\":3}]"
        search "[*].bar" "[{\"bar\": 1}, {\"bar\": 2}, {\"bar\": 3}]" `shouldBe` Right "[1,2,3]"
        search "[*].bar" "[{\"bar\": 1}, {\"bar\": 2}, {\"notbar\": 3}]" `shouldBe` Right "[1,2]"
        search "foo[*].bar" "{\"foo\": [{\"bar\": 1}, {\"bar\": 2}, {\"bar\": 3}]}" `shouldBe` Right "[1,2,3]"
        search "foo[*].bar" "{\"foo\": [{\"bar\": 1}, {\"bar\": 2}, {\"notbar\": 3}]}" `shouldBe` Right "[1,2]"
        search "foo[*].bar[0]" "{\"foo\": [{\"bar\": [1]}, {\"bar\": [2]}, {\"bar\": [3]}]}" `shouldBe` Right "[1,2,3]"
        search "a[*].b[*].c" "{\"a\": [{\"b\": [{\"c\": 1}, {\"c\": 2}]}, {\"b\": [{\"c\": 3}]}]}" `shouldBe` Right "[[1,2],[3]]"

    context "with an object projection" $
      it "applies the following expressions to each value in an object" $ do
        search "*" "{\"a\": 1, \"b\": 2, \"c\": 3}" `shouldBe` Right "[1,2,3]"
        search "foo.*" "{\"foo\": {\"a\": 1, \"b\": 2, \"c\": 3}}" `shouldBe` Right "[1,2,3]"
        search "*[0]" "{\"a\": [1], \"b\": [2], \"c\": [3]}" `shouldBe` Right "[1,2,3]"
        search "*[0]" "{\"a\": [1], \"b\": {}, \"c\": [3]}" `shouldBe` Right "[1,3]"
        search "foo.*[0]" "{\"foo\": {\"a\": [1], \"b\": [2], \"c\": [3]}}" `shouldBe` Right "[1,2,3]"
        search "foo.*[0].bar" "{\"foo\": {\"a\": [{\"bar\": 1}], \"b\": [{\"bar\": 2}]}}" `shouldBe` Right "[1,2]"
