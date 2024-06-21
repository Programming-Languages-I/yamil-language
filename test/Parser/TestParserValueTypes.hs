module Parser.TestParserValueTypes
        ( module Parser.TestParserValueTypes
        ) where

import           AST

import           Parser.ParserValueTypes

import           Test.Hspec

import           Text.Parsec

testParseIdentifier :: Spec
testParseIdentifier = describe "parseIdentifier" $ do
  it "parses underscore letter" $ do
    parse parseUnderscoreLetter "" "_hola" `shouldBe` Right "_hola"

  it "parses a valid identifier" $ do
    parse parseIdentifier "" "abc123" `shouldBe` Right "abc123"

  it "parses a single letter identifier" $ do
    parse parseIdentifier "" "a" `shouldBe` Right "a"

  it "fails on identifier starting with underscore" $ do
    parse parseIdentifier "" "a_a" `shouldBe` Right "a_a"

testParseLiterals :: Spec
testParseLiterals = describe "parseLiterals" $ do
  it "parses an integer literal" $ do
    parse parseIntLiteral "" "123" `shouldBe` Right (IntLiteral 123)

  it "parses a double literal with one decimal" $ do
    parse parseDoubleLiteral "" "123.4" `shouldBe` Right (DoubleLiteral 123.4)

  it "parses a double literal with three decimals" $ do
    parse parseDoubleLiteral "" "123.456" `shouldBe` Right (DoubleLiteral 123.456)

  it "parses a boolean literal True" $ do
    parse parseBoolLiteral "" "True" `shouldBe` Right (BoolLiteral True)

  it "parses a boolean literal False" $ do
    parse parseBoolLiteral "" "False" `shouldBe` Right (BoolLiteral False)

  it "parses a string literal" $ do
    parse parseStringLiteral "" "\"hello\"" `shouldBe` Right (StringLiteral "hello")

  it "parses a string literal with spaces and special char" $ do
    parse parseStringLiteral "" "\"hello world! \"" `shouldBe` Right (StringLiteral "hello world! ")

testParseValue :: Spec
testParseValue = describe "parseValue" $ do
  it "parses an integer value" $ do
    parse parseValue "" "123" `shouldBe` Right (VLiteral (IntLiteral 123))

  it "parses an identifier value" $ do
    parse parseValue "" "a12" `shouldBe` Right (VIdentifier "a12")

testParseType :: Spec
testParseType = describe "parseType" $ do
  it "parses an int type" $ do
    parse parseType "" ": int" `shouldBe` Right TInt

  it "parses an double type" $ do
    parse parseType "" ": double" `shouldBe` Right TDouble

  it "parses an bool type" $ do
    parse parseType "" ": bool" `shouldBe` Right TBool

  it "parses an string type" $ do
    parse parseType "" ": string" `shouldBe` Right TString

  it "parses an string type without spaces" $ do
    parse parseType "" ":string" `shouldBe` Right TString

testParseTypedIdentifier :: Spec
testParseTypedIdentifier = describe "parseTypedIdentifier" $ do
  it "parses an int typed identifier" $ do
    parse parseTypedIdentifier "" "b12: int" `shouldBe` Right (TypedIdentifier "b12" TInt)

  it "parses an double typed identifier" $ do
    parse parseTypedIdentifier "" "a_bc: double" `shouldBe` Right (TypedIdentifier "a_bc" TDouble)

  it "parses an bool typed identifier" $ do
    parse parseTypedIdentifier "" "cd31: bool" `shouldBe` Right (TypedIdentifier "cd31" TBool)

  it "parses an string typed identifier" $ do
    parse parseTypedIdentifier "" "a: string" `shouldBe` Right (TypedIdentifier "a" TString)

testParseValueTypes :: Spec
testParseValueTypes = do
  testParseIdentifier
  testParseLiterals
  testParseValue
  testParseType
  testParseTypedIdentifier
