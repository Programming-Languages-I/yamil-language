module Semantic.SymbolTable (module Semantic.SymbolTable) where

import           AST
import qualified Data.Map as Map

type SymbolTable = Map.Map String Symbol

data Scope = GLOBAL | LOCAL | BLOCK deriving (Show, Eq)

data BuiltInType = INTEGER | DOUBLE | BOOL | STRING deriving (Show, Eq)

data SymbolValue
  = IntSymbolValue Int
  | BoolSymbolValue Bool
  | DoubleSymbolValue Double
  | StringSymbolValue String
  deriving (Show, Eq)

data Symbol = Symbol String BuiltInType SymbolValue deriving (Show, Eq)

insertSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
insertSymbol name symbol symbolTable = Map.insert name symbol symbolTable

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = Map.lookup name table

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol name symbolTable = Map.delete name symbolTable

builtInTypeFromLiteral :: Literal -> BuiltInType
builtInTypeFromLiteral IntLiteral {} = INTEGER
builtInTypeFromLiteral DoubleLiteral {} = DOUBLE
builtInTypeFromLiteral BoolLiteral {} = BOOL
builtInTypeFromLiteral StringLiteral {} = STRING

builtInTypeFromType :: Type -> BuiltInType
builtInTypeFromType TInt = INTEGER
builtInTypeFromType TDouble = DOUBLE
builtInTypeFromType TBool = BOOL
builtInTypeFromType TString = STRING

nameFromIdentifier :: Identifier -> String
nameFromIdentifier (s) = s

symbolValueFromValue :: Value -> SymbolValue
symbolValueFromValue (VLiteral (IntLiteral value)) = IntSymbolValue value
symbolValueFromValue (VLiteral (BoolLiteral value)) = BoolSymbolValue value
symbolValueFromValue (VLiteral (DoubleLiteral value)) = DoubleSymbolValue value
symbolValueFromValue (VLiteral (StringLiteral value)) = StringSymbolValue value

valueFromSymbolValue :: SymbolValue -> Literal
valueFromSymbolValue (IntSymbolValue v) = IntLiteral v
valueFromSymbolValue (BoolSymbolValue v) = BoolLiteral v
valueFromSymbolValue (DoubleSymbolValue v) = DoubleLiteral v
valueFromSymbolValue (StringSymbolValue v) = StringLiteral v
