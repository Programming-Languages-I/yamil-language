module Interpreter.CodeTranspiler (writePascalFile, exampleLiterals, exampleVars) where

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

generatePascalProgram :: [TypedIdentifier] -> [Literal] -> Doc ann
generatePascalProgram vars literals =
  vsep [ pretty "program Yamil;"
       , pretty "var"
       , indent 2 (typedIdentifiersToPascal vars)
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , pretty "end." 
       ]

exampleLiterals :: [Literal] 
exampleLiterals = [IntLiteral 42, DoubleLiteral 3.14, StringLiteral "Hello, Pascal!"]

exampleVars :: [TypedIdentifier]
exampleVars = [TypedIdentifier "a" TInt, TypedIdentifier "b" TBool]

writePascalFile :: FilePath -> [TypedIdentifier] -> [Literal] -> IO ()
writePascalFile filePath vars literals = do
    let pascalCode = renderString $ layoutPretty defaultLayoutOptions $ generatePascalProgram vars literals
    writeFile filePath pascalCode
