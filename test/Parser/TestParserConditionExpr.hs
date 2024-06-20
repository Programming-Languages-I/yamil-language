module Parser.TestParserConditionExpr (module Parser.TestParserConditionExpr) where

import AST
import Parser.ParserConditionExpr
import Test.Hspec
import Text.Parsec


testParseCondition :: Spec
testParseCondition = describe "parseCondition" $ do
  it "parses a simple comparison condition (integer)" $ do
    parse parseCondition "" "a < 10" `shouldBe` Right (Condition (VIdentifier "a") LessThan (VLiteral (IntLiteral 10)))

  it "parses a comparison condition (double)" $ do
    parse parseCondition "" "x >= 3.14" `shouldBe` Right (Condition (VIdentifier "x") GreaterEqual (VLiteral (DoubleLiteral 3.14)))

  it "parses a comparison condition (string)" $ do
    parse parseCondition "" "name == \"Alice\"" `shouldBe` Right (Condition (VIdentifier "name") Equal (VLiteral (StringLiteral "Alice")))

testParseConditionAnd :: Spec
testParseConditionAnd = describe "parseConditionAnd" $ do
  it "parses a simple AND condition" $ do
    parse parseConditionAnd "" "a < 10 && b == true" `shouldBe` Right (ConditionAnd (Condition (VIdentifier "a") LessThan (VLiteral (IntLiteral 10))) (Condition (VIdentifier "b") Equal (VLiteral (BoolLiteral True))))

  it "parses a simple AND condition" $ do
    parse parseConditionAnd "" "x > 5 && y <= 100" `shouldBe` Right (ConditionAnd (Condition (VIdentifier "x") GreaterThan (VLiteral (IntLiteral 5))) (Condition (VIdentifier "y") LessEqual (VLiteral (IntLiteral 100))))
testParseConditionOr :: Spec
testParseConditionOr = describe "parseConditionOr" $ do
  it "parses a simple OR condition" $ do
    parse parseConditionOr "" "a == 1 || b != 2" `shouldBe` Right (ConditionOr (Condition (VIdentifier "a") Equal (VLiteral (IntLiteral 1))) (Condition (VIdentifier "b") NotEqual (VLiteral (IntLiteral 2))))

  it "parses a simple OR condition" $ do
    parse parseConditionOr "" "x < 0 || y >= 1" `shouldBe` Right (ConditionOr (Condition (VIdentifier "x") LessThan (VLiteral (IntLiteral 0))) (Condition (VIdentifier "y") GreaterEqual (VLiteral (IntLiteral 10))))
testParseConditionBool :: Spec
testParseConditionBool = describe "parseConditionBool" $ do
  it "parses a True boolean value" $ do
    parse parseConditionBool "" "True" `shouldBe` Right (ConditionBool True)

  it "parses a False boolean value" $ do
    parse parseConditionBool "" "False" `shouldBe` Right (ConditionBool False)


testParseIfExpr :: Spec
testParseIfExpr = describe "parseIfExpr" $ do
  it "parses a simple if-then expression" $ do
   parse parseIfExpr "" "if y == 0 then 1+1 \n 2+2 else  2+3 \n 5+7" `shouldBe` Right (IfExpr (Condition (VIdentifier "y") Equal (VLiteral (IntLiteral 0))) (ThenMainExpr (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 1))) (BinaryExpr (VLiteral (IntLiteral 2)) Add (VLiteral (IntLiteral 2)))) (ThenMainExpr (BinaryExpr (VLiteral (IntLiteral 2)) Add (VLiteral (IntLiteral 3))) (BinaryExpr (VLiteral (IntLiteral 5)) Add (VLiteral (IntLiteral 7)))))


testParserIfExpression :: Spec
testParserIfExpression = do
    testParseCondition
    testParseIfExpr
    testParseConditionBool
    testParseConditionOr
    testParseConditionAnd
