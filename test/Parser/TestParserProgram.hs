module Parser.TestParserProgram (module Parser.TestParserProgram) where

import AST
import Parser.ParserProgram

import Test.Hspec
import Text.Parsec

testParseProgramElement :: Spec
testParseProgramElement = describe "parseProgramElement" $ do
    it "parses a function" $ do
        parse parseProgramElement "" "def add(x: int, y: int) -> int = {\n x+y }"
            `shouldBe` Right
                (PEFunction
                    (Function
                        "add"
                        [TypedIdentifier "x" TInt, TypedIdentifier "y" TInt]
                        TInt
                        (FBody [FBExpr (BinaryExpr (VIdentifier "x") Add (VIdentifier "y"))])
                    )
                )

    it "parses a let statement" $ do
        parse parseProgramElement "" "let sum:int=1+2" `shouldBe` Right (PELetStatement (LetStatement (TypedIdentifier "sum" TInt) (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2)))))

    it "parses a function call" $ do
        parse parseProgramElement "" "let z:int = foo()" `shouldBe` Right (PELetStatement (LetStatement (TypedIdentifier "z" TInt) (FunctionCall "foo" [])))

    it "parses a program" $ do
        parse parseProgramElement "" "def add(x: int, y: int) -> int = {\n x+y }"
            `shouldBe` Right
                (PEFunction
                    (Function
                        "add"
                        [TypedIdentifier "x" TInt, TypedIdentifier "y" TInt]
                        TInt
                        (FBody [FBExpr (BinaryExpr (VIdentifier "x") Add (VIdentifier "y"))])
                    )
                )

        parse parseProgramElement "" "let sum:int=1+2" `shouldBe` Right (PELetStatement (LetStatement (TypedIdentifier "sum" TInt) (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2)))))

        parse parseProgramElement "" "let z:int = foo()" `shouldBe` Right (PELetStatement (LetStatement (TypedIdentifier "z" TInt) (FunctionCall "foo" [])))

    it "parses a function call" $ do
        parse parseProgram "" "let x: int = 5\ndef calculate(a: int, b: int) -> int = { \
        \let sum: int = add(a, b)\n\
        \let product: int = a * b\n\
        \add(sum, product) }\ncalculate(x, 5)\n" `shouldBe` Right
           (Program [PELetStatement (LetStatement (TypedIdentifier "x" TInt) (ValueExpr (VLiteral (IntLiteral 5)))), 
           PEFunction (Function "calculate" [TypedIdentifier "a" TInt, TypedIdentifier "b" TInt] TInt (FBody [FBLetStatement (LetStatement (TypedIdentifier "sum" TInt) (FunctionCall "add" [VIdentifier "a", VIdentifier "b"])), 
           FBLetStatement (LetStatement (TypedIdentifier "product" TInt) (BinaryExpr (VIdentifier "a") Multiply (VIdentifier "b"))), 
           FBExpr (FunctionCall "add" [VIdentifier "sum", VIdentifier "product"])])), 
           PEFunctionCall (FunctionCall "calculate" [VIdentifier "x", VLiteral (IntLiteral 5)])])


testParserProgram :: Spec
testParserProgram = do
    testParseProgramElement