module TestSymbolTable where

import           Test.Hspec
import           Semantic.SymbolTable
import qualified Data.Map as Map
import           AST

testInsertSymbolInST :: Spec
testInsertSymbolInST = describe "ST_insertSymbol" $ do
    it "inserts a symbol into the symbol table" $ do
      let symbol = Symbol "x" INTEGER (IntSymbolValue 10)
      let symbolTable = insertSymbol "x" symbol Map.empty
      Map.lookup "x" symbolTable `shouldBe` Just symbol

testLookUpSymbolInST :: Spec
testLookUpSymbolInST = describe "ST_lookupSymbol" $ do
    it "returns Just symbol if the symbol exists in the table" $ do
      let symbol = Symbol "y" BOOL (BoolSymbolValue True)
      let symbolTable = insertSymbol "y" symbol Map.empty
      lookupSymbol "y" symbolTable `shouldBe` Just symbol

    it "returns Nothing if the symbol does not exist in the table" $ do
      let symbolTable = Map.empty
      lookupSymbol "z" symbolTable `shouldBe` Nothing

testDeleteSymbolFromST :: Spec 
testDeleteSymbolFromST = describe "ST_deleteSymbol" $ do
    it "deletes a symbol from the symbol table" $ do
      let symbol = Symbol "w" STRING (StringSymbolValue "hello")
      let symbolTable = insertSymbol "w" symbol Map.empty
      let updatedTable = deleteSymbol "w" symbolTable
      lookupSymbol "w" updatedTable `shouldBe` Nothing

testBuiltInTypeFromLiteral :: Spec 
testBuiltInTypeFromLiteral = describe "ST_builtInTypeFromLiteral" $ do
    it "returns INTEGER for IntLiteral" $ do
      builtInTypeFromLiteral (IntLiteral 42) `shouldBe` INTEGER

    it "returns DOUBLE for DoubleLiteral" $ do
      builtInTypeFromLiteral (DoubleLiteral 3.14) `shouldBe` DOUBLE

    it "returns BOOL for BoolLiteral" $ do
      builtInTypeFromLiteral (BoolLiteral True) `shouldBe` BOOL

    it "returns STRING for StringLiteral" $ do
      builtInTypeFromLiteral (StringLiteral "test") `shouldBe` STRING

testSymbolValueFromValue :: Spec 
testSymbolValueFromValue = describe "ST_symbolValueFromValue" $ do
    it "returns IntSymbolValue for IntLiteral value" $ do
      symbolValueFromValue (VLiteral (IntLiteral 42)) `shouldBe` IntSymbolValue 42

    it "returns BoolSymbolValue for BoolLiteral value" $ do
      symbolValueFromValue (VLiteral (BoolLiteral True)) `shouldBe` BoolSymbolValue True

    it "returns DoubleSymbolValue for DoubleLiteral value" $ do
      symbolValueFromValue (VLiteral (DoubleLiteral 3.14)) `shouldBe` DoubleSymbolValue 3.14

    it "returns StringSymbolValue for StringLiteral value" $ do
      symbolValueFromValue (VLiteral (StringLiteral "test")) `shouldBe` StringSymbolValue "test"

testValueFromSymbolValue :: Spec
testValueFromSymbolValue = describe "ST_valueFromSymbolValue" $ do
    it "returns IntLiteral for IntSymbolValue" $ do
      valueFromSymbolValue (IntSymbolValue 42) `shouldBe` IntLiteral 42

    it "returns BoolLiteral for BoolSymbolValue" $ do
      valueFromSymbolValue (BoolSymbolValue True) `shouldBe` BoolLiteral True

    it "returns DoubleLiteral for DoubleSymbolValue" $ do
      valueFromSymbolValue (DoubleSymbolValue 3.14) `shouldBe` DoubleLiteral 3.14

    it "returns StringLiteral for StringSymbolValue" $ do
      valueFromSymbolValue (StringSymbolValue "test") `shouldBe` StringLiteral "test"

testSymbolTable :: Spec 
testSymbolTable = do
    testInsertSymbolInST
    testDeleteSymbolFromST
    testBuiltInTypeFromLiteral
    testSymbolValueFromValue
    testValueFromSymbolValue
