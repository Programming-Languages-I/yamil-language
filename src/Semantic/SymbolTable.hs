module Semantic.SymbolTable(
                    insertSymbol
                  , lookupSymbol
                  , deleteSymbol
                  , SymbolTable
                  , Scope(..)
                  , Symbol(..)
                  , SymbolValue(SymbolValue)
                  , Builtype(..)
                  , nameFromIdentifier
                  , builtypeFromType
                  ) where

import qualified Data.Map as Map
import AST

type SymbolTable = Map.Map String Symbol

data Scope = GLOBAL|
             LOCAL |
             BLOCK deriving (Show,Eq)

data Builtype = Int | Bool | Double | String deriving (Show, Eq)

data Symbol = Symbol String Builtype SymbolValue deriving (Show, Eq)

data SymbolValue = SymbolValue Int deriving(Show, Eq)

insertSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
insertSymbol name symbol symbolTable = Map.insert name symbol symbolTable

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = Map.lookup name table

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol name symbolTable = Map.delete name symbolTable

builtypeFromType:: Type -> Builtype
builtypeFromType TInt = Int
builtypeFromType TBool = Bool
builtypeFromType TDouble = Double
builtypeFromType TString = String

nameFromIdentifier :: Identifier -> String
nameFromIdentifier s = s