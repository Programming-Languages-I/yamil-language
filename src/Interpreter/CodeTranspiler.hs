module Interpreter.CodeTranspiler 
        ( writePascalFile
        , exampleLiterals
        , exampleVars
        , exampleLetStatements
        , exampleConditionExpr
        , exampleThenExpr
        , exampleIfExpr
        , exampleBinaryExpr
        , exampleLambdaExp
        , exampleExprs
        ) where

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

typedIdentifiersToPascalWithoutLastSemicolon :: [TypedIdentifier] -> Doc ann
typedIdentifiersToPascalWithoutLastSemicolon vars =
    vsep $ map (\(idx, var) -> 
        if idx == length vars - 1 
        then identifierToPascal (name var) <> pretty ": " <> typeToPascal (typ var) 
        else typedIdentifierToPascal var
    ) (zip [0..] vars)
  where
    name (TypedIdentifier n _) = n
    typ (TypedIdentifier _ t) = t

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
exprToPascal (BinaryExpr val1 op val2) = binaryExprToPascal val1 op val2
exprToPascal (ValueExpr value) = valueToPascal value
exprToPascal (IfExpr conds thens1 thens2) = pretty "if" <+> conditionExprToPascal conds <+> pretty "then" <+> thenExprToPascal thens1

exprListsToPascal :: [Expr] -> Doc ann
exprListsToPascal exprs = vsep (map exprToPascal exprs)

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
    parens (typedIdentifiersToPascalWithoutLastSemicolon params) <>
    pretty ";" <+>
    pretty "begin" <+>
    indent 2 (exprToPascal expr) <+>
    pretty "end;"

binaryExprToPascal :: Value -> ArithmeticOperator -> Value -> Doc ann
binaryExprToPascal val1 op val2 =
    valueToPascal val1 <+> arithmeticOperatorPascal op <+> valueToPascal val2

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

patternToPascal :: Pattern -> Doc ann
patternToPascal (PLiteral lit) = literalToPascal lit
patternToPascal (PIdentifier ident) = pretty ident

patternMatchToPascalCase :: PatternMatch -> Doc ann
patternMatchToPascalCase (PatternMatchExp pattern expr) =
    patternToPascal pattern <> pretty ":" <+> pretty "WriteLn" <> parens (exprToPascal expr) <> pretty ";"
patternMatchToPascalCase (PatternMatchLit pattern lit) =
    patternToPascal pattern <> pretty ":" <+> pretty "WriteLn" <> parens (literalToPascal lit) <> pretty ";"

otherwiseMatchToPascalElse :: OtherwiseMatch -> Doc ann
otherwiseMatchToPascalElse (OtherwiseExp expr) =
    pretty "else" <+> pretty "WriteLn" <> parens (exprToPascal expr) <> pretty ";"
otherwiseMatchToPascalElse (OtherwiseLit lit) =
    pretty "else" <+> pretty "WriteLn" <> parens (literalToPascal lit) <> pretty ";"

generatePascalProgram :: [TypedIdentifier] -> [Literal] -> [LetStatement] -> [LambdaExpr] -> [Expr] -> Doc ann
generatePascalProgram vars literals lets lambdas exprs =
  vsep [ pretty "program Yamil;"
       , pretty "var"
       , indent 2 (typedIdentifiersToPascal vars)
       , indent 2 (vsep (map letStatementToPascal lets))
       , indent 2 (vsep  (map lambdaToPascal lambdas))
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , indent 2 (exprListsToPascal exprs)
       , pretty "end." 
       ]

exampleLiterals :: [Literal] 
exampleLiterals = [IntLiteral 42, DoubleLiteral 3.14, StringLiteral "Hello, Pascal!"]

exampleVars :: [TypedIdentifier]
exampleVars = [TypedIdentifier "a" TInt, TypedIdentifier "b" TBool]

exampleLetStatements :: [LetStatement]
exampleLetStatements = [LetStatement (TypedIdentifier "y" TInt) (ValueExpr (VLiteral (IntLiteral 10)))]

exampleLambdaExp :: [LambdaExpr]
exampleLambdaExp = [LambdaExpr "operation" [TypedIdentifier "x" TInt,TypedIdentifier "y" TInt ,TypedIdentifier "z" TInt] (ValueExpr (VIdentifier "x"))]

exampleConditionExpr :: ConditionExpr
exampleConditionExpr = Condition (VLiteral (IntLiteral 5)) GreaterThan (VLiteral (IntLiteral 3))

exampleThenExpr :: ThenExpr
exampleThenExpr = ThenMainExpr (ValueExpr (VLiteral (IntLiteral 5)))

exampleExprs :: [Expr]
exampleExprs = [IfExpr exampleConditionExpr exampleThenExpr exampleThenExpr, BinaryExpr (VIdentifier "a") Add (VIdentifier "b")]

exampleIfExpr :: Expr
exampleIfExpr = IfExpr exampleConditionExpr exampleThenExpr exampleThenExpr

exampleBinaryExpr :: Expr
exampleBinaryExpr = BinaryExpr (VIdentifier "a") Add (VIdentifier "b")

writePascalFile :: FilePath -> [TypedIdentifier] -> [Literal] -> [LetStatement] -> [LambdaExpr] -> [Expr] -> IO ()
writePascalFile filePath vars literals lets lambda exprs = do
    let pascalCode = renderString $ layoutPretty defaultLayoutOptions $ generatePascalProgram vars literals lets lambda exprs
    writeFile filePath pascalCode
