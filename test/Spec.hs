import Test.Hspec

import Data.Either
import Data.List

import JMESPath

main :: IO ()
main = hspec $
  describe "JMESPath.search" $ do
    context "when syntax is invalid" $
      it "fails" $ do
        search "" "{}" `shouldBe` Left "Syntax error: unexpected end of input"
        fromLeft "" (search ".foo" "{}") `shouldSatisfy` isPrefixOf "Syntax error: unexpected token '.'"

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
        search "*[0]" "{\"a\": [1], \"b\": [2], \"c\": [3]}" `shouldBe` Right "[1,2,3]"
        search "*[0]" "{\"a\": [1], \"b\": {}, \"c\": [3]}" `shouldBe` Right "[1,3]"
        search "foo.*" "{\"foo\": {\"a\": 1, \"b\": 2, \"c\": 3}}" `shouldBe` Right "[1,2,3]"
        search "foo.*[0]" "{\"foo\": {\"a\": [1], \"b\": [2], \"c\": [3]}}" `shouldBe` Right "[1,2,3]"
        search "foo.*[0].bar" "{\"foo\": {\"a\": [{\"bar\": 1}], \"b\": [{\"bar\": 2}]}}" `shouldBe` Right "[1,2]"
        search "foo.*[0].bar" "{\"foo\": {\"a\": [{\"bar\": 1}], \"b\": [{\"bar\": 2}]}}" `shouldBe` Right "[1,2]"
        search "foo.*.bar.*" "{\"foo\": {\"a\": {\"bar\": {\"x\": 1}}}}" `shouldBe` Right "[[1]]"

    context "with a flattening expression" $
      it "applies the following expressions to each element of a list, and flattens the result" $ do
        search "[]" "[1, [2, 3], 4]" `shouldBe` Right "[1,2,3,4]"
        search "[]" "[1, [2, null], 3, null]" `shouldBe` Right "[1,2,3]"
        search "foo[]" "{\"foo\":[[1, 2, 3]]}" `shouldBe` Right "[1,2,3]"
        search "foo[]" "{\"foo\":\"\"}" `shouldBe` Right "null"
        search "[][0]" "[[[1, 2], [3, 4]], [[5, 6], [7, 8]], [[9]], [[10]]]" `shouldBe` Right "[1,3,5,7,9,10]"
        search "foo[][0]" "{\"foo\": [[[1]], [[2]], [[3]]]}" `shouldBe` Right "[1,2,3]"
        search "foo[]" "{\"foo\":[[1, 2, 3]]}" `shouldBe` Right "[1,2,3]"
        search "a[].b[]" "{\"a\": [{\"b\": [[1], [2]]}, {\"b\": [[3]]}]}" `shouldBe` Right "[[1],[2],[3]]"
        search "a[].b[].c" "{\"a\": [{\"b\": [{\"c\": [1]}, {\"c\": [2]}]}, {\"b\": [{\"c\": [3]}]}]}" `shouldBe` Right "[[1],[2],[3]]"
        search "foo[][]" "{\"foo\":[[1, 2, 3]]}" `shouldBe` Right "[1,2,3]"
        search "foo[][].bar" "{\"foo\":[[{\"bar\": 1}, {\"bar\": 2}, {\"bar\": 3}]]}" `shouldBe` Right "[1,2,3]"

    context "with a slice expression" $
      it "extracts the corresponding elements" $ do
        search "[1:3]" "[1, 2, 3, 4]" `shouldBe` Right "[2,3]"
        search "[-3:-1]" "[1, 2, 3, 4]" `shouldBe` Right "[2,3]"
        search "[0:10]" "[1, 2, 3]" `shouldBe` Right "[1,2,3]"
        search "[10:11]" "[1, 2, 3]" `shouldBe` Right "[]"
        search "[-4:2]" "[1, 2, 3]" `shouldBe` Right "[1,2]"
        search "[-4:-5]" "[1, 2, 3]" `shouldBe` Right "[]"
        search "[3:1]" "[1, 2, 3, 4]" `shouldBe` Right "[]"
        search "[0::1]" "[1, 2, 3]" `shouldBe` Right "[1,2,3]"
        search "[::-1]" "[1, 2, 3]" `shouldBe` Right "[3,2,1]"
        search "bar[1:3]" "{\"bar\": [1, 2, 3, 4]}" `shouldBe` Right "[2,3]"
        search "foo[*][1:3]" "{\"foo\": [[1, 2, 3, 4]]}" `shouldBe` Right "[[2,3]]"
        search "foo[*].bar[1:3]" "{\"foo\": [{\"bar\": [1, 2, 3, 4]}]}" `shouldBe` Right "[[2,3]]"
        search "[3:7:2]" "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]" `shouldBe` Right "[3,5]"
        search "[8:2:-2]" "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]" `shouldBe` Right "[8,6,4]"
        search "[0:1:0]" "[0]" `shouldBe` Left "Error: slice step cannot be 0"
        search "foo[3:7:2]" "{\"foo\": [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]}" `shouldBe` Right "[3,5]"
        search "foo[*][3:7:2]" "{\"foo\": [[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]}" `shouldBe` Right "[[3,5]]"
        search "foo[*].bar[3:7:2]" "{\"foo\": [{\"bar\": [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]}]}" `shouldBe` Right "[[3,5]]"
        search "foo[1:].bar" "{\"foo\": [{\"bar\": 1}, {\"bar\": 2}, {\"bar\": 3}]}" `shouldBe` Right "[2,3]"

    context "with a pipe expression" $
      it "stops the propagation of the projection" $ do
        search "foo[*].bar | [0]" "{\"foo\": [{\"bar\": [1, 2]}, {\"bar\": [3, 4]}]}" `shouldBe` Right "[1,2]"
        search "foo| bar | baz" "{\"foo\": {\"bar\": {\"baz\": true}}}" `shouldBe` Right "true"

    context "with a multiselect list" $
      it "returns a list of the results of evalutaing the single expressions" $ do
        search "[one, two]" "{\"one\": 1, \"two\": 2}" `shouldBe` Right "[1,2]"
        search "foo.[one, two]" "{\"foo\": {\"one\": 1, \"two\": 2}}" `shouldBe` Right "[1,2]"
        search "foo[*].[one, two]" "{\"foo\": [{\"one\": 1, \"two\": 2}]}" `shouldBe` Right "[[1,2]]"
        search "foo[*][0].[one, two]" "{\"foo\": [[{\"one\": 1, \"two\": 2}]]}" `shouldBe` Right "[[1,2]]"
        search "foo.[bar]" "{}" `shouldBe` Right "null"

    context "with a multiselect hash" $
      it "returns an object with the same keys and expression results as values" $ do
        search "{one: one, two: two}" "{\"one\": 1, \"two\": 2, \"three\": 3}" `shouldBe` Right "{\"two\":2,\"one\":1}"
        search "{one: one, two: two}" "null" `shouldBe` Right "null"
        search "foo.{one: one, two: two}" "{\"foo\": {\"one\": 1, \"two\": 2, \"three\": 3}}" `shouldBe` Right "{\"two\":2,\"one\":1}"
        search "foo.{one: one, two: two}" "{}" `shouldBe` Right "null"
        search "foo[*].{one: one}" "{\"foo\": [{\"one\": 1, \"two\": 2}]}" `shouldBe` Right "[{\"one\":1}]"
        search "foo[*][0].{one: one}" "{\"foo\": [[{\"one\": 1, \"two\": 2}]]}" `shouldBe` Right "[{\"one\":1}]"

    context "with boolean expressions" $ do
      it "applies the usual precedence rules" $ do
        search "a || b && c" "{\"a\": true, \"b\": true, \"c\": false}" `shouldBe` Right "true"
        search "a && b || c" "{\"a\": false, \"b\": false, \"c\": true}" `shouldBe` Right "true"
        search "!a && b" "{\"a\": false, \"b\": false}" `shouldBe` Right "false"

      context "||" $
        it "returns the first non-false expression" $ do
          search "foo || bar" "{\"foo\": 42}" `shouldBe` Right "42"
          search "foo || bar" "{\"bar\": 42}" `shouldBe` Right "42"
          search "foo || bar" "{\"foo\": [], \"bar\": 42}" `shouldBe` Right "42"
          search "foo || bar" "{\"foo\": {}, \"bar\": 42}" `shouldBe` Right "42"
          search "foo || bar" "{\"foo\": \"\", \"bar\": 42}" `shouldBe` Right "42"
          search "foo || bar" "{\"foo\": false, \"bar\": 42}" `shouldBe` Right "42"

      context "&&" $
        it "returns the last non-false expression" $ do
          search "foo && bar" "{\"foo\": true, \"bar\": 42}" `shouldBe` Right "42"
          search "foo && bar" "{\"bar\": 42}" `shouldBe` Right "null"

      context "!" $
        it "returns the logical negation of the expression" $
          search "!foo" "{\"foo\": true}" `shouldBe` Right "false"

      context "parenthesis" $
        it "gives precedence to expressions in parenthesis" $ do
          search "(a || b) && c" "{\"a\": true, \"b\": true, \"c\": false}" `shouldBe` Right "false"
          search "a && (b || c)" "{\"a\": false, \"b\": false, \"c\": true}" `shouldBe` Right "false"

      context "comparators" $ do
        it "applies the usual precedence rules" $ do
          search "one < two && three > one" "{\"one\": 1, \"two\": 2, \"three\": 3}" `shouldBe` Right "true"
          search "one <= two && three >= one" "{\"one\": 1, \"two\": 2, \"three\": 3}" `shouldBe` Right "true"
          search "one < two == three > one" "{\"one\": 1, \"two\": 2, \"three\": 3}" `shouldBe` Right "true"
          search "one < two != three > one" "{\"one\": 1, \"two\": 2, \"three\": 3}" `shouldBe` Right "false"

        context "==" $
          it "returns true if the two values are equal" $ do
            search "a == b" "{\"a\": 42, \"b\": 42}" `shouldBe` Right "true"
            search "a == b" "{\"a\": 42, \"b\": 43}" `shouldBe` Right "false"

        context "!=" $
          it "returns true if the two values are not equal" $ do
            search "a != b" "{\"a\": 42, \"b\": 42}" `shouldBe` Right "false"
            search "a != b" "{\"a\": 42, \"b\": 43}" `shouldBe` Right "true"

        context "<" $
          it "returns true if the first number is less than the second" $ do
            search "a < b" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "true"
            search "b < a" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "false"
            search "a < b" "{\"a\": 0, \"b\": []}" `shouldBe` Right "null"

        context ">" $
          it "returns true if the first number is greater than the second" $ do
            search "a > b" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "false"
            search "b > a" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "true"
            search "a > b" "{\"a\": 0, \"b\": []}" `shouldBe` Right "null"

        context "<=" $
          it "returns true if the first number is less than or equal the second" $ do
            search "a <= b" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "true"
            search "a <= b" "{\"a\": 1, \"b\": 1}" `shouldBe` Right "true"
            search "b <= a" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "false"
            search "a <= b" "{\"a\": 0, \"b\": []}" `shouldBe` Right "null"

        context ">=" $
          it "returns true if the first number is greater than or equal the second" $ do
            search "a >= b" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "false"
            search "a >= b" "{\"a\": 1, \"b\": 1}" `shouldBe` Right "true"
            search "b >= a" "{\"a\": 0, \"b\": 1}" `shouldBe` Right "true"
            search "a >= b" "{\"a\": 0, \"b\": []}" `shouldBe` Right "null"

    context "with a filter expression" $
      it "keeps the element that make the expression return a truthy value" $ do
        search "[?a == b]" "[{\"a\": 1, \"b\": 2}, {\"a\": 1, \"b\": 1}]" `shouldBe` Right "[{\"a\":1,\"b\":1}]"
        search "foo[?a == b]" "{\"foo\": [{\"a\": 1, \"b\": 2}, {\"a\": 1, \"b\": 1}]}" `shouldBe` Right "[{\"a\":1,\"b\":1}]"
        search "[*][?a == b]" "[[{\"a\": 1, \"b\": 2}, {\"a\": 1, \"b\": 1}]]" `shouldBe` Right "[[{\"a\":1,\"b\":1}]]"
        search "[*].foo[?a == b]" "[{\"foo\": [{\"a\": 1, \"b\": 2}, {\"a\": 1, \"b\": 1}]}]" `shouldBe` Right "[[{\"a\":1,\"b\":1}]]"
        search "[?first == last].first" "[{\"first\": 1, \"last\": 2}, {\"first\": 1, \"last\": 1}]" `shouldBe` Right "[1]"

    context "with literals" $
      it "parses the JSON" $ do
        search "`{\"foo\": \"bar\"}`" "{}" `shouldBe` Right "{\"foo\":\"bar\"}"
        search "'foo'" "{}" `shouldBe` Right "\"foo\""

    context "with a current-node expression" $
      it "returns the current element" $
        search "[?@ < `2`]" "[1, 2, 3]" `shouldBe` Right "[1]"

    context "with a function call" $ do
      it "evaluates the function call" $ do
        search "abs(foo)" "{\"foo\": -1}" `shouldBe` Right "1"
        search "[].abs(@)" "[-1, 2, -3]" `shouldBe` Right "[1,2,3]"
        search "[].foo.abs(@)" "[{\"foo\": -1}, {\"foo\": -2}, {\"foo\": -3}]" `shouldBe` Right "[1,2,3]"

      context "when the function does not exist" $
        it "returns an error" $
          search "foo(bar)" "{}" `shouldBe` Left "undefined function 'foo'"

      context "when the number of arguments is wrong" $
        it "returns an error" $
          search "abs(foo, bar)" "{}" `shouldBe` Left "abs: invalid arity, expected 1 argument"

      context "with nested calls" $
        it "works" $
          search "abs(avg(@))" "[-1, -2, -3]" `shouldBe` Right "2"

      describe "abs" $
        it "calculates a number's absolute value" $ do
          search "abs(@)" "-1" `shouldBe` Right "1"
          search "abs(@)" "1" `shouldBe` Right "1"
          search "abs(@)" "\"bar\"" `shouldBe` Left "abs: invalid type of argument '\"bar\"'"

      describe "avg" $
        it "calculates the average of a list of numbers" $ do
          search "avg(@)" "[10, 15, 20]" `shouldBe` Right "15"
          search "avg(@)" "[10, \"foo\", 20]" `shouldBe` Left "avg: invalid type of value '\"foo\"'"
          search "avg(@)" "[]" `shouldBe` Right "null"
          search "avg(@)" "false" `shouldBe` Left "avg: invalid type of argument 'false'"

      describe "contains" $ do
        context "with a value of the wrong type" $
          it "returns an error" $
            search "contains(@, 'foo')" "false" `shouldBe` Left "contains: invalid type of argument 'false'"

        context "with an array" $
          it "checks if the array contains the provided element" $ do
            search "contains(@, 'foo')" "[\"bar\"]" `shouldBe` Right "false"
            search "contains(@, 'foo')" "[\"foo\"]" `shouldBe` Right "true"

        context "with a string" $
          it "checks if the string contains the provided substring" $ do
            search "contains(@, 'baz')" "\"foobar\"" `shouldBe` Right "false"
            search "contains(@, 'bar')" "\"foobar\"" `shouldBe` Right "true"
            search "contains(@, `false`)" "\"foobar\"" `shouldBe` Left "contains: invalid type of argument 'false'"

      describe "ceil" $
        it "rounds a number to the next integer" $ do
          search "ceil(@)" "1.001" `shouldBe` Right "2"
          search "ceil(@)" "1" `shouldBe` Right "1"
          search "ceil(@)" "\"foo\"" `shouldBe` Left "ceil: invalid type of argument '\"foo\"'"

      describe "ends_with" $
        it "checks if a string ends with the provided suffix" $ do
          search "ends_with(@, 'bar')" "\"foo\"" `shouldBe` Right "false"
          search "ends_with(@, 'bar')" "\"foobar\"" `shouldBe` Right "true"
          search "ends_with(@, `123`)" "\"foobar\"" `shouldBe` Left "ends_with: invalid type of argument '123'"
          search "ends_with(@, 'bar')" "123" `shouldBe` Left "ends_with: invalid type of argument '123'"

      describe "floor" $
        it "rounds a number to the previous integer" $ do
          search "floor(@)" "1.001" `shouldBe` Right "1"
          search "floor(@)" "1" `shouldBe` Right "1"
          search "floor(@)" "\"foo\"" `shouldBe` Left "floor: invalid type of argument '\"foo\"'"

      describe "join" $
        it "joins an array of strings using a glue string" $ do
          search "join(',', @)" "[\"foo\", \"bar\"]" `shouldBe` Right "\"foo,bar\""
          search "join(',', @)" "[\"foo\", 42]" `shouldBe` Left "join: invalid type of value '42'"
          search "join(',', @)" "false" `shouldBe` Left "join: invalid type of argument 'false'"
          search "join(`false`, @)" "[\"foo\", \"bar\"]" `shouldBe` Left "join: invalid type of argument 'false'"
