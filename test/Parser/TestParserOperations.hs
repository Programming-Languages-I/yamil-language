module Parser.TestParserOperations
        ( module Parser.TestParserOperations
        ) where

import           AST

import           Parser.ParserOperations (parseArithmeticOperator)

import           Test.Hspec

import           Text.Parsec             (parse)

testParseArithmeticOperator :: Spec
testParseArithmeticOperator = describe "parseArithmeticOperator" $ do
  it "parses an addition operator" $ do
    parse parseArithmeticOperator "" "+ " `shouldBe` Right Add

  it "parses a subtraction operator" $ do
    parse parseArithmeticOperator "" "- " `shouldBe` Right Subtract

  it "parses a multiplication operator" $ do
    parse parseArithmeticOperator "" "* " `shouldBe` Right Multiply

  it "parses a division operator" $ do
    parse parseArithmeticOperator "" "/ " `shouldBe` Right Divide

testParseOperations :: Spec
testParseOperations = do
  testParseArithmeticOperator

