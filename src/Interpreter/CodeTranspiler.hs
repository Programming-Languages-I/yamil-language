module Interpreter.CodeTranspiler (writePascalFile, exampleLiterals, exampleVars, exampleLetStatements, exampleConditionExpr, exampleThenExpr) where

import AST
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Hspec (example)

identifierToPascal :: Identifier -> Doc ann
identifierToPascal = pretty

literalToPascal :: Literal -> Doc ann
literalToPascal (IntLiteral n)    = pretty n
-- literalToPascal (BoolLiteral b)   = if b then "True" else "False"
literalToPascal (DoubleLiteral d) = pretty d
literalToPascal (StringLiteral s) = dquotes $ pretty s

literalsToPascal :: [Literal] -> Doc ann
literalsToPascal literals = vsep (map literalToPascal literals)

typeToPascal :: Type -> Doc ann
typeToPascal TInt    = pretty "integer"
typeToPascal TBool   = pretty "boolean"
typeToPascal TDouble = pretty "real"
typeToPascal TString = pretty "string"

typedIdentifierToPascal :: TypedIdentifier -> Doc ann
typedIdentifierToPascal (TypedIdentifier name t) =
    identifierToPascal name <> pretty ": " <> typeToPascal t <> pretty ";"

typedIdentifiersToPascal :: [TypedIdentifier] -> Doc ann
typedIdentifiersToPascal vars = vsep (map typedIdentifierToPascal vars)

valueToPascal :: Value -> Doc ann
valueToPascal (VLiteral lit)          = literalToPascal lit
valueToPascal (VIdentifier ident)     = identifierToPascal ident
valueToPascal (VFunctionCall ident args) = 
    identifierToPascal ident <> parens (hsep (punctuate (pretty ", ") (map valueToPascal args)))

valuesToPascal :: [Value] -> Doc ann
valuesToPascal values = vsep (map (writeln . valueToPascal) values)
  where
    writeln doc = pretty "writeln(" <> doc <> pretty ");"

exprToPascal :: Expr -> Doc ann
exprToPascal (ValueExpr value) = valueToPascal value


letStatementToPascal :: LetStatement -> Doc ann
letStatementToPascal (LetStatement (TypedIdentifier name t) expr) =
    identifierToPascal name <+> pretty ":" <+> typeToPascal t <> pretty " = " <> exprToPascal expr <> pretty ";"

comparisonOperator :: ComparisonOperator -> Doc ann
comparisonOperator Equal = pretty "="
comparisonOperator NotEqual = pretty "<>"
comparisonOperator LessThan = pretty "<"
comparisonOperator GreaterThan = pretty ">"
comparisonOperator LessEqual = pretty "<="
comparisonOperator GreaterEqual = pretty ">=" 


arithmeticOperatorPascal :: ArithmeticOperator -> Doc ann
arithmeticOperatorPascal Add    = pretty "+"
arithmeticOperatorPascal Subtract    = pretty "-"
arithmeticOperatorPascal Multiply    = pretty "*"
arithmeticOperatorPascal Divide    = pretty "/"

conditionExprToPascal :: ConditionExpr -> Doc ann
conditionExprToPascal (Condition value1 op value2) =
    valueToPascal value1 <+> comparisonOperator op <+> valueToPascal value2
conditionExprToPascal (ConditionAnd cond1 cond2) =
    conditionExprToPascal cond1 <+> pretty "&" <+> conditionExprToPascal cond2
conditionExprToPascal (ConditionOr cond1 cond2) =
    conditionExprToPascal cond1 <+> pretty "|" <+> conditionExprToPascal cond2
conditionExprToPascal (ConditionBool bool) = pretty $ boolToString bool
  where
    boolToString True = "True"
    boolToString False = "False"

thenExprToPascal :: ThenExpr -> Doc ann
thenExprToPascal (ThenMainExpr expr) = exprToPascal expr
thenExprToPascal (ThenLiteral lit) = literalToPascal lit
thenExprToPascal (ThenIdentifier ident) = identifierToPascal ident

generatePascalProgram :: [TypedIdentifier] -> [Literal] -> [LetStatement] -> ConditionExpr -> ThenExpr -> Doc ann
generatePascalProgram vars literals lets conds thens =
  vsep [ pretty "program Yamil;"
       , pretty "var"
       , indent 2 (typedIdentifiersToPascal vars)
       , indent 2 (vsep (map letStatementToPascal lets))
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , indent 2 (conditionExprToPascal conds)
       , indent 2 (thenExprToPascal thens)
       , pretty "end." 
       ]

exampleLiterals :: [Literal] 
exampleLiterals = [IntLiteral 42, DoubleLiteral 3.14, StringLiteral "Hello, Pascal!"]

exampleVars :: [TypedIdentifier]
exampleVars = [TypedIdentifier "a" TInt, TypedIdentifier "b" TBool]

exampleLetStatements :: [LetStatement]
exampleLetStatements = [LetStatement (TypedIdentifier "y" TInt) (ValueExpr (VLiteral (IntLiteral 10)))]

exampleConditionExpr :: ConditionExpr
exampleConditionExpr = Condition (VLiteral (IntLiteral 5)) GreaterThan (VLiteral (IntLiteral 3))

exampleThenExpr :: ThenExpr
exampleThenExpr = ThenMainExpr (ValueExpr (VLiteral (IntLiteral 5)))

writePascalFile :: FilePath -> [TypedIdentifier] -> [Literal] -> [LetStatement] -> ConditionExpr -> ThenExpr -> IO ()
writePascalFile filePath vars literals lets conds thens = do
    let pascalCode = renderString $ layoutPretty defaultLayoutOptions $ generatePascalProgram vars literals lets conds thens
    writeFile filePath pascalCode

