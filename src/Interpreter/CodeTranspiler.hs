module Interpreter.CodeTranspiler (writePascalFile, exampleLiterals) where

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

generatePascalProgram :: [Literal] -> Doc ann
generatePascalProgram literals =
  vsep [ pretty "program Yamil;"
       , pretty "begin"
       , indent 2 (literalsToPascal literals)
       , pretty "end." 
       ]

exampleLiterals :: [Literal] 
exampleLiterals = [IntLiteral 42, DoubleLiteral 3.14, StringLiteral "Hello, Pascal!"]

writePascalFile :: FilePath -> [Literal] -> IO ()
writePascalFile filePath literals = do
    let pascalCode = renderString $ layoutPretty defaultLayoutOptions $ generatePascalProgram literals
    writeFile filePath pascalCode
