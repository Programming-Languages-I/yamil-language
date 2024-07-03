module Semantic.SemanticAnalyzer(module Semantic.SemanticAnalyzer) where

import qualified Semantic.SymbolTable as ST
import qualified AST as AST 
import qualified Data.Map as Map

analyzeTypeIndentifier :: AST.TypedIdentifier -> ST.SymbolTable -> (AST.TypedIdentifier, ST.SymbolTable)
analyzeTypeIndentifier typeIdent@(AST.TypedIdentifier identifiers typeVar) symbTable =
     (typeIdent, insertOrError symbTable identifiers)
  where
    insertOrError table identifier =
      let name = ST.nameFromIdentifier identifier
          symbol = ST.Symbol name (ST.builtInTypeFromType typeVar) (ST.IntSymbolValue 0)
      in case ST.lookupSymbol name table of
           Just _  -> error (typeIdentAlreadyDefinedError name)
           Nothing -> ST.insertSymbol name symbol table

analyzeTypeIndentifiers :: ST.SymbolTable -> [AST.TypedIdentifier] -> ST.SymbolTable
analyzeTypeIndentifiers = foldl (\table element -> snd (analyzeTypeIndentifier element table))

typeIdentAlreadyDefinedError :: String -> String
typeIdentAlreadyDefinedError varName = "Variable already defined: " ++ varName

analyzeLetStatment :: AST.LetStatement -> ST.SymbolTable -> (AST.LetStatement, ST.SymbolTable)
--to do : Analisis de expresiones aca 
analyzeLetStatment statment@(AST.LetStatement typeIndet _) table = 
     (statment, snd (analyzeTypeIndentifier typeIndet table))

analyzeFuntionOption :: AST.FunctionBodyOpts -> ST.SymbolTable -> (AST.FunctionBodyOpts, ST.SymbolTable)
--to do : Analisis de expresiones aca 
analyzeFuntionOption options@(AST.FBExpr _ ) _ = (options, Map.empty)
analyzeFuntionOption options@(AST.FBLetStatement letStatment) symbTable = (options, snd (analyzeLetStatment letStatment symbTable))
analyzeFuntionOption options@(AST.FBEmpty ) _ = (options, Map.empty)

analyzeFuntionOptions :: ST.SymbolTable -> [AST.FunctionBodyOpts] -> ST.SymbolTable
analyzeFuntionOptions = foldl (\table element -> snd (analyzeFuntionOption element table))

analyzeFuntionBody :: AST.FunctionBody -> ST.SymbolTable -> (AST.FunctionBody, ST.SymbolTable)
analyzeFuntionBody fbody@(AST.FBody options) table = (fbody, analyzeFuntionOptions table options)
--to do : Analisis de pattern aca 
analyzeFuntionBody fbody@(AST.FBPatternMatch _) _ = (fbody, Map.empty)
--to do : Analisis de lambda aca 
analyzeFuntionBody fbody@(AST.FBLambdaExpr _) _ = (fbody, Map.empty)

analyzeFuntion :: AST.Function -> ST.SymbolTable -> (AST.Function, ST.SymbolTable)
analyzeFuntion function@(AST.Function fIndentifier typeIndents funcType body ) symTable =
     (function, 
      insertFunction (snd (analyzeFuntionBody body (analyzeTypeIndentifiers symTable typeIndents))) fIndentifier) 
    where 
        insertFunction table identifier = 
            let name = ST.nameFromIdentifier identifier
                symbol = ST.Symbol name (ST.builtInTypeFromType funcType) (ST.IntSymbolValue 0)
            in case ST.lookupSymbol name table of
                Just _  -> error (typeIdentAlreadyDefinedError name)
                Nothing -> ST.insertSymbol name symbol table

analyzeProgramElem :: AST.ProgramElement -> ST.SymbolTable -> (AST.ProgramElement, ST.SymbolTable)
analyzeProgramElem programElem@(AST.PEFunction funtion ) table = (programElem, snd (analyzeFuntion funtion table))
analyzeProgramElem programElem@(AST.PELetStatement statment ) table = (programElem, snd (analyzeLetStatment statment table))
--to do : Analisis de functions Call aca 
analyzeProgramElem programElem@(AST.PEFunctionCall _ ) _ = (programElem, Map.empty)

analyzeProgramElems :: ST.SymbolTable -> [AST.ProgramElement] -> ST.SymbolTable
analyzeProgramElems = foldl (\table element -> snd (analyzeProgramElem element table))

analyzeProgram :: AST.Program -> ST.SymbolTable -> (AST.Program, ST.SymbolTable)
analyzeProgram program@(AST.Program elements) symbolTable =
    (program, analyzeProgramElems symbolTable elements)

