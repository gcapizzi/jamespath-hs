module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import JMESPath.Ast
import qualified JMESPath.Json as Json

searchValue :: Expression -> Json.Value -> Either String Json.Value
searchValue Root document = Right document
searchValue (KeyExpression key Root) document = Right $ Json.lookupKey key document
searchValue (KeyExpression key expression) document = do
    value <- searchValue expression document
    searchValue (KeyExpression key Root) value
searchValue (IndexExpression index Root) document = Right $ Json.lookupIndex index document
searchValue (IndexExpression index expression) document = do
    value <- searchValue expression document
    searchValue (IndexExpression index Root) value
searchValue (ArrayProjectExpression Root expression) document = Json.mapArray (searchValue expression) document
searchValue (ArrayProjectExpression left right) document = do
    value <- searchValue left document
    searchValue (ArrayProjectExpression Root right) value
searchValue (ObjectProjectExpression Root expression) document = Json.mapObject (searchValue expression) document
searchValue (ObjectProjectExpression left right) document = do
    value <- searchValue left document
    searchValue (ObjectProjectExpression Root right) value
searchValue (FlattenExpression Root expression) document = Json.flatMapArray (searchValue expression) document
searchValue (FlattenExpression left right) document = do
    value <- searchValue left document
    searchValue (FlattenExpression Root right) value
searchValue (SliceExpression from to step Root expression) document = do
    slice <- Json.slice from to step document
    Json.mapArray (searchValue expression) slice
searchValue (SliceExpression from to step left right) document = do
    value <- searchValue left document
    searchValue (SliceExpression from to step Root right) value
searchValue (PipeExpression left right) document = do
    value <- searchValue left document
    searchValue right value
searchValue (MultiSelectList expressions Root) document =
    if Json.isNull document
        then Right Json.null
        else do
            values <- mapM (`searchValue` document) expressions
            Right $ Json.array values
searchValue (MultiSelectList expressions expression) document = do
    value <- searchValue expression document
    searchValue (MultiSelectList expressions Root) value
searchValue (MultiSelectHash pairs Root) document =
    if Json.isNull document
        then Right Json.null
        else do
            valuePairs <- mapM (mapM (`searchValue` document)) pairs
            Right $ Json.object valuePairs
searchValue (MultiSelectHash pairs expression) document = do
    value <- searchValue expression document
    searchValue (MultiSelectHash pairs Root) value
searchValue (OrExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    if Json.isFalsy leftValue
        then Right rightValue
        else Right leftValue
searchValue (AndExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    if Json.isTruthy leftValue
        then Right rightValue
        else Right leftValue
searchValue (NotExpression expression) document = do
    value <- searchValue expression document
    Right $ Json.bool $ Json.isFalsy value
searchValue (EqualExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    Right $ Json.equal leftValue rightValue
searchValue (NotEqualExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    Right $ Json.notEqual leftValue rightValue
searchValue (LessThanExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    Right $ Json.lessThan leftValue rightValue
searchValue (GreaterThanExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    Right $ Json.greaterThan leftValue rightValue
searchValue (LessThanOrEqualExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    Right $ Json.lessThanOrEqual leftValue rightValue
searchValue (GreaterThanOrEqualExpression left right) document = do
    leftValue <- searchValue left document
    rightValue <- searchValue right document
    Right $ Json.greaterThanOrEqual leftValue rightValue
searchValue (FilterExpression filter Root) document = Json.filterArray (searchValue filter) document
searchValue (FilterExpression filter expression) document = do
    value <- searchValue expression document
    searchValue (FilterExpression filter Root) value
