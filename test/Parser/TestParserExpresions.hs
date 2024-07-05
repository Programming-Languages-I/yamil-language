module Parser.TestParserExpresions
        ( module Parser.TestParserExpresions
        ) where

import           AST

import           Parser.ParserExpresions
import           Parser.ParserValueTypes

import           Test.Hspec

import           Text.Parsec

testParseFunctionCall :: Spec
testParseFunctionCall = describe "parseFunctionCall" $ do
    it "parses a function call with no arguments" $ do
        parse parseFunctionCall "" "foo()" `shouldBe` Right (FunctionCall "foo" [])

    it "parses a function call with one argument" $ do
        parse parseFunctionCall "" "bar(123)" `shouldBe` Right (FunctionCall "bar" [VLiteral (IntLiteral 123)])

    it "parses a function call with multiple arguments without spaces" $ do
        parse parseFunctionCall "" "baz(1,2,3)" `shouldBe` Right (FunctionCall "baz" [VLiteral (IntLiteral 1), VLiteral (IntLiteral 2), VLiteral (IntLiteral 3)])

    it "parses a function call with multiple arguments with spaces in the middle" $ do
        parse parseFunctionCall "" "baz(1, 2, 3)" `shouldBe` Right (FunctionCall "baz" [VLiteral (IntLiteral 1), VLiteral (IntLiteral 2), VLiteral (IntLiteral 3)])

testParseBinaryExpr :: Spec
testParseBinaryExpr = describe "parseBinaryExpr" $ do
    it "parses a binary expression with addition" $ do
        parse parseBinaryExpr "" "1+2" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2)))

    it "parses a binary expression with subtraction" $ do
        parse parseBinaryExpr "" "3-4" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 3)) Subtract (VLiteral (IntLiteral 4)))

    it "parses a binary expression with multiplication" $ do
        parse parseBinaryExpr "" "5*6" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 5)) Multiply (VLiteral (IntLiteral 6)))

    it "parses a binary expression with division" $ do
        parse parseBinaryExpr "" "7/8" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 7)) Divide (VLiteral (IntLiteral 8)))

    it "parses a binary expression with addition with spaces in the middle" $ do
        parse parseBinaryExpr "" "1 + 2" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2)))

    it "parses a binary expression with subtraction with spaces in the middle" $ do
        parse parseBinaryExpr "" "3 - 4" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 3)) Subtract (VLiteral (IntLiteral 4)))

    it "parses a binary expression with multiplication with spaces in the middle" $ do
        parse parseBinaryExpr "" "5 * 6" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 5)) Multiply (VLiteral (IntLiteral 6)))

    it "parses a binary expression with division with spaces in the middle" $ do
        parse parseBinaryExpr "" "7 / 8" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 7)) Divide (VLiteral (IntLiteral 8)))

testParseExpr :: Spec
testParseExpr = describe "parseExpr" $ do
    it "parses a function call" $ do
        parse parseExpr "" "foo()" `shouldBe` Right (FunctionCall "foo" [])

    it "parses a binary expression" $ do
        parse parseExpr "" "1+2" `shouldBe` Right (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2)))

    it "parses a binary expression" $ do
        parse parseExpr "" "x + y" `shouldBe` Right (BinaryExpr (VIdentifier "x") Add (VIdentifier "y"))

testParseParamsValues :: Spec
testParseParamsValues = describe "parseParamsValues" $ do
    it "parses a list of values with no arguments" $ do
        parse parseParamsValues "" "()" `shouldBe` Right []

    it "parses a list of values with one argument" $ do
        parse parseParamsValues "" "(123)" `shouldBe` Right [VLiteral (IntLiteral 123)]

    it "parses a list of values with multiple arguments" $ do
        parse parseParamsValues "" "(1,2)" `shouldBe` Right [VLiteral (IntLiteral 1), VLiteral (IntLiteral 2)]

testParseLambda :: Spec
testParseLambda = do
    it "parses a lambda expression with functionCall" $ do
        parse parseLambda "" "lambda operation (x:double) -> foo()" `shouldBe` Right (LambdaExpr "operation" [TypedIdentifier "x" TDouble] (FunctionCall "foo" []) )

    it "parses a lambda expression with BinaryExpr" $ do
        parse parseLambda "" "lambda operations(x:double) -> 5 * 6" `shouldBe` Right (LambdaExpr "operations" [TypedIdentifier "x" TDouble] (BinaryExpr (VLiteral (IntLiteral 5)) Multiply (VLiteral (IntLiteral 6))) )

testParserExpresions :: Spec
testParserExpresions = do
    testParseFunctionCall
    testParseBinaryExpr
    testParseExpr
    testParseParamsValues
    testParseLambda
