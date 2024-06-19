module Parser.TestParserValueTypes(module Parser.TestParserValueTypes) where 

import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)
import Parser.ParserValueTypes
import AST

main :: IO ()
main = hspec $ do
  describe "parseIdentifier" $ do
    it "parses a valid identifier" $ do
      parse parseIdentifier "" "a1_b2" `shouldBe` Right "a1_b2"
      
    it "parses a single letter identifier" $ do
      parse parseIdentifier "" "a" `shouldBe` Right "a"
      
    it "fails on identifier starting with underscore" $ do
      parse parseIdentifier "" "a_a" `shouldBe` Right "a_a"

  describe "parseIntLiteral" $ do
    it "parses an integer literal" $ do
      parse parseIntLiteral "" "123" `shouldBe` Right (IntLiteral 123)

  describe "parseDoubleLiteral" $ do
    it "parses a double literal" $ do
      parse parseDoubleLiteral "" "123.456" `shouldBe` Right (DoubleLiteral 123.456)

  describe "parseBoolLiteral" $ do
    it "parses a boolean literal True" $ do
      parse parseBoolLiteral "" "True" `shouldBe` Right (BoolLiteral True)
      
    it "parses a boolean literal False" $ do
      parse parseBoolLiteral "" "False" `shouldBe` Right (BoolLiteral False)

  describe "parseStringLiteral" $ do
    it "parses a string literal" $ do
      parse parseStringLiteral "" "hello" `shouldBe` Right (StringLiteral "hello")

    it "parses a string literal with spaces and special char" $ do
      parse parseStringLiteral "" "hello" `shouldBe` Right (StringLiteral "\"hello world! \"")

  describe "parseValue" $ do
    it "parses an integer value" $ do
      parse parseValue "" "123" `shouldBe` Right (VLiteral (IntLiteral 123))
      
    it "parses an identifier value" $ do
      parse parseValue "" "a1_b2" `shouldBe` Right (VIdentifier "a1_b2")

  describe "parseTypedIdentifier" $ do
    it "parses a typed identifier" $ do
      parse parseTypedIdentifier "" "a: Int" `shouldBe` Right (TypedIdentifier "a" TInt)
