module Parser.TestParserThenExpr (module Parser.TestParserThenExpr) where

import AST
import Parser.ParserExpresions
import Test.Hspec
import Text.Parsec

testParseThenMainExpr :: Spec
testParseThenMainExpr = describe "parseThenMainExpr" $ do
  it "parses a valid then expression with two main expressions" $ do
    parse parseThenMainExpr "" "1 + 2 \n 1+4" `shouldBe` Right (ThenMainExpr (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2))) (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 4))))

  it "parses a then expression with a literal" $ do
    parse parseThenMainExpr "" "123 * 123 \n 123 / 123" `shouldBe`Right (ThenMainExpr (BinaryExpr (VLiteral (IntLiteral 123)) Multiply (VLiteral (IntLiteral 123))) (BinaryExpr (VLiteral (IntLiteral 123)) Divide (VLiteral (IntLiteral 123))))

  it "parses a then expression with an identifier" $ do
    parse parseThenMainExpr "" "foo() \n 123+1 " `shouldBe` Right (ThenMainExpr (FunctionCall "foo" []) (BinaryExpr (VLiteral (IntLiteral 123)) Add (VLiteral (IntLiteral 1))))
testParseThenLiteral :: Spec
testParseThenLiteral = describe "parseThenLiteral" $ do
  it "parses a then literal expression with an integer literal" $ do
    parse parseThenLiteral "" "123" `shouldBe` Right (ThenLiteral (IntLiteral 123))

  it "parses a then literal expression with a boolean literal" $ do
    parse parseThenLiteral "" "True" `shouldBe` Right (ThenLiteral (BoolLiteral True))

  it "parses a then literal expression with a string literal" $ do
    parse parseThenLiteral "" "\"hello\"" `shouldBe` Right (ThenLiteral (StringLiteral "hello"))

testParseThenIdentifier :: Spec
testParseThenIdentifier = describe "parseThenIdentifier" $ do
  it "parses a then identifier expression" $ do
    parse parseThenIdentifier "" "abc123" `shouldBe` Right (ThenIdentifier "abc123")

  it "parses a then identifier expression with underscore" $ do
    parse parseThenIdentifier "" "identifier" `shouldBe` Right (ThenIdentifier "identifier")

testParseThenExpression :: Spec
testParseThenExpression = do 
  testParseThenIdentifier 
  testParseThenLiteral
  testParseThenMainExpr
