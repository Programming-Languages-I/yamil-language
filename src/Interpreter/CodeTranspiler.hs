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
        , examplePatternMatches
        , exampleFunction,
        programToPascal
        ) where

import AST
import Prettyprinter
import Prettyprinter.Render.String (renderString)

identifierToPascal :: Identifier -> Doc ann
identifierToPascal = pretty

literalToPascal :: Literal -> Doc ann
literalToPascal (IntLiteral n)    = pretty n
literalToPascal (BoolLiteral b)   = if b then pretty "True" else pretty "False"
literalToPascal (DoubleLiteral d) = pretty d
literalToPascal (StringLiteral s) = squotes $ pretty s

literalsToPascal :: [Literal] -> Doc ann
literalsToPascal literals = vsep (map literalToPascal literals)

typeToPascal :: Type -> Doc ann
typeToPascal TInt    = pretty "integer"
typeToPascal TBool   = pretty "boolean"
typeToPascal TDouble = pretty "real"
typeToPascal TString = pretty "string"

typedIdentifierToPascal :: TypedIdentifier -> Doc ann
typedIdentifierToPascal (TypedIdentifier name t) =
    identifierToPascal name <> pretty ": " <> typeToPascal t

typedIdentifiersToPascal :: [TypedIdentifier] -> Doc ann
typedIdentifiersToPascal vars = vsep (map (\var -> typedIdentifierToPascal var <> semi) vars)

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
exprToPascal (FunctionCall ident args) = 
    pretty ident <> parens (hsep (punctuate comma (map valueToPascal args))) <> pretty ";"
exprToPascal (IfExpr conds thens1 thens2) = 
    pretty "if" <+> conditionExprToPascal conds <+> 
    pretty "\nthen" <+> thenExprToPascal thens1 <+>
    pretty "else" <+> thenExprToPascal thens2
exprToPascal (BinaryExpr val1 op val2) = binaryExprToPascal val1 op val2
exprToPascal (ValueExpr value) = valueToPascal value

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

patternMatchToPascalCase :: PatternMatch -> Identifier -> Doc ann
patternMatchToPascalCase (PatternMatchExp pattern expr) ident =
    patternToPascal pattern <> pretty ":" <+> pretty ident <+> pretty ":="  <>  exprToPascal expr <> pretty ";"
patternMatchToPascalCase (PatternMatchLit pattern lit) ident =
    patternToPascal pattern <> pretty ":" <+> pretty ident <+> pretty ":=" <> literalToPascal lit <> pretty ";"

otherwiseMatchToPascalElse :: OtherwiseMatch -> Doc ann
otherwiseMatchToPascalElse (OtherwiseExp expr) =
    pretty "else" <+> pretty "WriteLn" <> parens (exprToPascal expr) <> pretty ";"
otherwiseMatchToPascalElse (OtherwiseLit lit) =
    pretty "else" <+> pretty "WriteLn" <> parens (literalToPascal lit) <> pretty ";"

patternMatchesToPascalCase :: PatternMatches -> Identifier -> Doc ann
patternMatchesToPascalCase (FullPatternMatch patternMatches otherwiseMatch) ident =
    pretty "case" <+> identifierToPascal "p" <+> pretty "of" <> line <>
    indent 2 (vsep (map (\pm -> patternMatchToPascalCase pm ident) patternMatches)) <> line <>
    indent 2 (otherwiseMatchToPascalElse otherwiseMatch) <> line <>
    pretty "end;"

extractLetStatementsFromFunc :: FunctionBody -> [LetStatement]
extractLetStatementsFromFunc (FBody opts) = concatMap extractLetFromOpts opts
extractLetStatementsFromFunc (FBPatternMatch _) = []
extractLetStatementsFromFunc (FBLambdaExpr _) = []

extractLetFromOpts :: FunctionBodyOpts -> [LetStatement]
extractLetFromOpts (FBExpr _) = []
extractLetFromOpts (FBLetStatement letStmt) = [letStmt]
extractLetFromOpts FBEmpty = []

letStatementsToTypedIdentifiers :: [LetStatement] -> [TypedIdentifier]
letStatementsToTypedIdentifiers = map (\(LetStatement tid _) -> tid)

functionToPascal :: Function -> Doc ann
functionToPascal (Function ident args functionType body) =
    let
        letStmts = extractLetStatementsFromFunc body
        localVars = letStatementsToTypedIdentifiers letStmts
    in
    vsep [ pretty "function" <+> pretty ident <> parens (hsep (punctuate semi (map typedIdentifierToPascal args))) <> colon <+> typeToPascal functionType <> semi
         , if null localVars then emptyDoc else pretty "var"
         , if null localVars then emptyDoc else indent 2 (typedIdentifiersToPascal localVars)
         , pretty "begin"
         , indent 2 (functionBodyToPascal body ident)
         , pretty "end;"
         ]

functionBodyToPascal :: FunctionBody -> Identifier -> Doc ann
functionBodyToPascal (FBody opts) name = vsep (map functionBodyOptsToPascal opts) <+> exprListsToPascalFromFunc name (extractExprsF opts)
    where
        extractExprsF :: [FunctionBodyOpts] -> [Expr]
        extractExprsF [] = []
        extractExprsF (FBExpr expr : rest) = expr : extractExprsF rest
        extractExprsF (_ : rest) = extractExprsF rest
functionBodyToPascal (FBPatternMatch patternMatches) ident = 
    patternMatchesToPascalCase patternMatches ident
functionBodyToPascal (FBLambdaExpr lambdaExpr) _ = lambdaToPascal lambdaExpr
    
letStatementInFuncToPascal :: LetStatement -> Doc ann
letStatementInFuncToPascal (LetStatement (TypedIdentifier name _) expr) =
    identifierToPascal name <+> pretty ":=" <+> exprToPascal expr <> pretty ";"

exprListsToPascalFromFunc :: Identifier -> [Expr] -> Doc ann
exprListsToPascalFromFunc _ [] = emptyDoc
exprListsToPascalFromFunc name exprs = vsep (map exprToPascal (init exprs) ++ [returnExprToPascal name (last exprs)])
  where
    returnExprToPascal :: Identifier -> Expr -> Doc ann
    returnExprToPascal name expr = case expr of
        IfExpr conds thens1 thens2 -> 
            pretty "if" <+> conditionExprToPascal conds <+> 
            pretty "then" <+> align (pretty name  <+> pretty ":= " <> thenExprToPascal thens1) <+>
            pretty "else" <+> align (pretty name  <+> pretty ":= " <> thenExprToPascal thens2)
        _ -> pretty name  <+> pretty ":= " <> exprToPascal expr <> semi

functionBodyOptsToPascal :: FunctionBodyOpts -> Doc ann
functionBodyOptsToPascal (FBExpr expr) = emptyDoc
functionBodyOptsToPascal (FBLetStatement letStmt) = letStatementInFuncToPascal letStmt
functionBodyOptsToPascal FBEmpty = emptyDoc

extractVars :: ProgramElement -> [TypedIdentifier]
extractVars (PEFunction (Function _ args _ _)) = args
extractVars _ = []

extractLiterals :: ProgramElement -> [Literal]
extractLiterals (PEFunction (Function _ _ _ (FBody bodyOpts))) = concatMap bodyOptLiterals bodyOpts
extractLiterals _ = []

bodyOptLiterals :: FunctionBodyOpts -> [Literal]
bodyOptLiterals (FBExpr (ValueExpr (VLiteral lit))) = [lit]
bodyOptLiterals _ = []

extractLetStatements :: ProgramElement -> [LetStatement]
extractLetStatements (PELetStatement letStmt) = [letStmt]
extractLetStatements _ = []

extractLambdas :: ProgramElement -> [LambdaExpr]
extractLambdas (PEFunction (Function _ _ _ (FBLambdaExpr lambda))) = [lambda]
extractLambdas _ = []

extractExprs :: ProgramElement -> [Expr]
extractExprs (PEFunctionCall expr) = [expr]
extractExprs _ = []

extractFunctions :: ProgramElement -> [Function]
extractFunctions (PEFunction func) = [func]
extractFunctions _ = []

collectTopLevelFunctionCalls :: Program -> [Expr]
collectTopLevelFunctionCalls (Program elems) = concatMap extractFunctionCalls elems
  where
    extractFunctionCalls :: ProgramElement -> [Expr]
    extractFunctionCalls (PEFunction _) = []
    extractFunctionCalls (PELetStatement _) = []
    extractFunctionCalls (PEFunctionCall expr) = [expr]


programToPascal :: Program -> Doc ann
programToPascal (Program elems) =
  vsep [ pretty "program Yamil;"
       , pretty "var"
       , indent 2 (vsep (map letStatementToPascal lets))
       , indent 2 (vsep (map functionToPascal functions))
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , indent 2 (exprListsToPascal exprs)
       , indent 2 (vsep (map exprToPascal topLevelFunctionCalls))
       , pretty "end."
       ]
  where
    vars = concatMap extractVars elems
    literals = concatMap extractLiterals elems
    lets = concatMap extractLetStatements elems
    lambdas = concatMap extractLambdas elems
    exprs = concatMap extractExprs elems
    functions = concatMap extractFunctions elems
    topLevelFunctionCalls = collectTopLevelFunctionCalls (Program elems)

programElementToPascal :: ProgramElement -> Doc ann
programElementToPascal (PEFunction func) = functionToPascal func
programElementToPascal (PELetStatement letStmt) = letStatementToPascal letStmt
programElementToPascal (PEFunctionCall expr) = exprToPascal expr

generatePascalProgram :: [TypedIdentifier] -> [Literal] -> [LetStatement] -> [LambdaExpr] -> [Expr] -> PatternMatches -> [Function] -> Doc ann
generatePascalProgram vars literals lets lambdas exprs patternMatches functions =
  vsep [ pretty "program Yamil;"
       , pretty "var"
       , indent 2 (typedIdentifiersToPascal vars)
       , indent 2 (vsep (map letStatementToPascal lets))
       , indent 2 (vsep  (map lambdaToPascal lambdas))
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , indent 2 (exprListsToPascal exprs)
       , indent 2 (patternMatchesToPascalCase patternMatches "h")
       , indent 2 (vsep (map functionToPascal functions))
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
exampleExprs = [(FunctionCall "match_letters" [VLiteral (StringLiteral "Haskell")]), IfExpr exampleConditionExpr exampleThenExpr exampleThenExpr, BinaryExpr (VIdentifier "a") Add (VIdentifier "b")]

exampleIfExpr :: Expr
exampleIfExpr = IfExpr exampleConditionExpr exampleThenExpr exampleThenExpr

exampleBinaryExpr :: Expr
exampleBinaryExpr = BinaryExpr (VIdentifier "a") Add (VIdentifier "b")

examplePatternMatches :: PatternMatches
examplePatternMatches = FullPatternMatch
  [ PatternMatchLit (PLiteral (IntLiteral 0)) (StringLiteral "Zero")
  , PatternMatchLit (PLiteral (IntLiteral 1)) (StringLiteral "One")
  , PatternMatchLit (PLiteral (IntLiteral 2)) (StringLiteral "Two")
  , PatternMatchLit (PLiteral (IntLiteral 3)) (StringLiteral "Three")
  , PatternMatchLit (PLiteral (IntLiteral 4)) (StringLiteral "Four")
  , PatternMatchLit (PLiteral (IntLiteral 5)) (StringLiteral "Five")
  , PatternMatchLit (PLiteral (IntLiteral 6)) (StringLiteral "Six")
  , PatternMatchLit (PLiteral (IntLiteral 7)) (StringLiteral "Seven")
  , PatternMatchLit (PLiteral (IntLiteral 8)) (StringLiteral "Eight")
  , PatternMatchLit (PLiteral (IntLiteral 9)) (StringLiteral "Nine")
  , PatternMatchLit (PLiteral (IntLiteral 10)) (StringLiteral "Ten")
  ] (OtherwiseLit (StringLiteral "Not Found"))

exampleFunction :: [Function]
exampleFunction = [Function "add" 
                    [TypedIdentifier "x" TInt, TypedIdentifier "y" TInt] 
                        TInt 
                        (FBody [FBExpr (BinaryExpr (VIdentifier "x") Add (VIdentifier "y"))])]

writePascalFile :: FilePath -> Program -> IO ()
writePascalFile filePath program = do
    let pascalCode = renderString $ layoutPretty defaultLayoutOptions $ programToPascal program
    writeFile filePath pascalCode
