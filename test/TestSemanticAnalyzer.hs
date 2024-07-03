module TestSemanticAnalyzer ( module TestSemanticAnalyzer
        ) where

import           Test.Hspec
import           Semantic.SemanticAnalyzer
import qualified Semantic.SymbolTable as ST
import qualified Data.Map as Map
import           AST
    
defaultSymbolTable :: ST.SymbolTable
defaultSymbolTable = Map.empty

-- Test for analyzeTypeIndentifier
testTypeIdentifier :: Spec
testTypeIdentifier = describe "analyzeTypeIndentifier" $ do
    it "adds a new symbol to the symbol table" $ do
        let typedId = TypedIdentifier "x" TInt
        let expectedMap = Map.fromList [("x", ST.Symbol "x" ST.INTEGER (ST.IntSymbolValue 0))]
        analyzeTypeIndentifier typedId defaultSymbolTable `shouldBe` (typedId, expectedMap)

-- Test for analyzeLetStatement
testLetStatement :: Spec
testLetStatement = describe "analyzeLetStatement" $ do
    it "adds a let statement to the symbol table" $ do
        let typedId = TypedIdentifier "y" TInt
        let letStatement = LetStatement typedId (ValueExpr (VLiteral (IntLiteral 10)))
        let expectedMap = Map.fromList [("y", ST.Symbol "y" ST.INTEGER (ST.IntSymbolValue 0))]
        analyzeLetStatment letStatement defaultSymbolTable `shouldBe` (letStatement, expectedMap)

-- Test for analyzeFunctionOption
testFunctionOption :: Spec
testFunctionOption = describe "analyzeFunctionOption" $ do
    it "analyzes an FBLetStatement and updates the symbol table" $ do
        let typedId = TypedIdentifier "z" TInt
        let letStatement = LetStatement typedId (ValueExpr (VLiteral (IntLiteral 20)))
        let functionOption = FBLetStatement letStatement
        let expectedMap = Map.fromList [("z", ST.Symbol "z" ST.INTEGER (ST.IntSymbolValue 0))]
        analyzeFuntionOption functionOption defaultSymbolTable `shouldBe` (functionOption, expectedMap)

-- Test for analyzeFunctionBody
testFunctionBody :: Spec
testFunctionBody = describe "analyzeFunctionBody" $ do
    it "analyzes a function body with let statements and updates the symbol table" $ do
        let typedId = TypedIdentifier "w" TInt
        let letStatement = LetStatement typedId (ValueExpr (VLiteral (IntLiteral 30)))
        let functionBody = FBody [FBLetStatement letStatement]
        let expectedMap = Map.fromList [("w", ST.Symbol "w" ST.INTEGER (ST.IntSymbolValue 0))]
        analyzeFuntionBody functionBody defaultSymbolTable `shouldBe` (functionBody, expectedMap)



-- Test for analyzeFunction
testFunction :: Spec
testFunction = describe "analyzeFunction" $ do
    it "analyzes a function and updates the symbol table" $ do
        let typedId = TypedIdentifier "b" TInt
        let typedIds = TypedIdentifier "c" TInt
        let letStatement = LetStatement typedId (ValueExpr (VLiteral (IntLiteral 40)))
        let functionBody = FBody [FBLetStatement letStatement]
        let function = Function "f" [typedIds] TInt functionBody
        let expectedMap = Map.fromList [("b", ST.Symbol "b" ST.INTEGER (ST.IntSymbolValue 0))
                                        ,("c", ST.Symbol "c" ST.INTEGER (ST.IntSymbolValue 0))
                                        ,("f", ST.Symbol "f" ST.INTEGER (ST.IntSymbolValue 0))]
        analyzeFuntion function defaultSymbolTable `shouldBe` (function, expectedMap)

-- Test for analyzeProgram
testProgram :: Spec
testProgram = describe "analyzeProgram" $ do
    it "analyzes a program with multiple elements and updates the symbol table" $ do
        let typedId1 = TypedIdentifier "b" TInt
        let typedId2 = TypedIdentifier "c" TInt
        let typedId3 = TypedIdentifier "d" TInt
        let letStatement1 = LetStatement typedId1 (ValueExpr (VLiteral (IntLiteral 50)))
        let letStatement2 = LetStatement typedId3 (ValueExpr (VLiteral (IntLiteral 50)))
        let functionBody = FBody [FBLetStatement letStatement1]
        let function = Function "g" [typedId2] TInt functionBody
        let functionCall = FunctionCall "g" [(VLiteral (IntLiteral 50))]
        let programElements = [PEFunction function, PELetStatement letStatement2, PEFunctionCall functionCall]
        let program = Program programElements
        let expectedMap = Map.fromList [("b", ST.Symbol "b" ST.INTEGER (ST.IntSymbolValue 0))
                                        ,("c", ST.Symbol "c" ST.INTEGER (ST.IntSymbolValue 0))
                                        ,("d", ST.Symbol "d" ST.INTEGER (ST.IntSymbolValue 0))
                                        ,("g", ST.Symbol "g" ST.INTEGER (ST.IntSymbolValue 0))]
        analyzeProgram program defaultSymbolTable `shouldBe` (program, expectedMap)


testSemanticAnalyzer :: Spec 
testSemanticAnalyzer = do
    testTypeIdentifier
    testLetStatement
    testFunctionOption
    testFunctionBody
    testFunction
    testProgram
