module Parser.TestParserFunction (module Parser.TestParserFunction) where

import AST
import Test.Hspec
import Text.Parsec
import Parser.ParserFunction
testParseLetStatement :: Spec
testParseLetStatement = describe "parseLetStatement" $ do
    it "parses a let statement with sum" $ do
        parse parseLetStatement "" "let sum:int=1+2" `shouldBe` Right (LetStatement (TypedIdentifier "sum" TInt) (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2))))

    it "parses a let statement with no spaces" $ do
        parse parseLetStatement "" "let cba:int=5 * 2" `shouldBe` Right (LetStatement (TypedIdentifier "cba" TInt) (BinaryExpr (VLiteral (IntLiteral 5)) Multiply (VLiteral (IntLiteral 2))))

    it "parses a let statement with Division" $ do
        parse parseLetStatement "" "let x_a:int= 8/2" `shouldBe` Right (LetStatement (TypedIdentifier "x_a" TInt) (BinaryExpr (VLiteral (IntLiteral 8)) Divide (VLiteral (IntLiteral 2))))

    it "parses a let statement with Subtract" $ do
        parse parseLetStatement "" "let x:int = 3 - 4" `shouldBe` Right (LetStatement (TypedIdentifier "x" TInt) (BinaryExpr (VLiteral (IntLiteral 3)) Subtract (VLiteral (IntLiteral 4))))

    it "parses a let statement with a function call" $ do
        parse parseLetStatement "" "let z:int = foo()" `shouldBe` Right (LetStatement (TypedIdentifier "z" TInt) (FunctionCall "foo" []))

testParserLetStatement :: Spec
testParserLetStatement = do
    testParseLetStatement
