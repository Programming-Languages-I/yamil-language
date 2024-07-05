module Interpreter.CodeTranspiler (writePascalFile, exampleLiterals, exampleVars, exampleLetStatements,exampleLambdaExp) where

import AST
import Prettyprinter
import Prettyprinter.Render.String (renderString)

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

lambdaToPascal :: LambdaExpr -> Doc ann
lambdaToPascal (LambdaExpr name params expr) =
    pretty "procedure" <+> identifierToPascal name <>
    parens (typedIdentifiersToPascal params) <>
    pretty ";" <+>
    pretty "begin" <+>
    indent 2 (exprToPascal expr) <+>
    pretty "end;"

generatePascalProgram :: [TypedIdentifier] -> [Literal] -> [LetStatement] -> [LambdaExpr] -> Doc ann
generatePascalProgram vars literals lets lambdas =
  vsep [ pretty "program Yamil;"
       , pretty "var"
       , indent 2 (typedIdentifiersToPascal vars)
       , indent 2 (vsep (map letStatementToPascal lets))
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , vsep (map lambdaToPascal lambdas)
       , pretty "end." 
       ]

exampleLiterals :: [Literal] 
exampleLiterals = [IntLiteral 42, DoubleLiteral 3.14, StringLiteral "Hello, Pascal!"]

exampleVars :: [TypedIdentifier]
exampleVars = [TypedIdentifier "a" TInt, TypedIdentifier "b" TBool]

exampleLetStatements :: [LetStatement]
exampleLetStatements = [LetStatement (TypedIdentifier "y" TInt) (ValueExpr (VLiteral (IntLiteral 10)))]

exampleLambdaExp :: [LambdaExpr]
exampleLambdaExp = [LambdaExpr "NAME" [TypedIdentifier "x" TInt] (ValueExpr (VLiteral (IntLiteral 3)))]


writePascalFile :: FilePath -> [TypedIdentifier] -> [Literal] -> [LetStatement] -> [LambdaExpr] -> IO ()
writePascalFile filePath vars literals lets lambdas = do
    let pascalCode = renderString $ layoutPretty defaultLayoutOptions $ generatePascalProgram vars literals lets lambdas
    writeFile filePath pascalCode
