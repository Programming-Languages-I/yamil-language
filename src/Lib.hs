module Lib
        ( someFunc
        ) where

import           Parser.ParserProgram as P
import           Interpreter.CodeTranspiler as CT

import           Text.Parsec

someFunc :: IO ()
someFunc = do
        content <- readFile "./app/resources/code.yamil"
        let result = parse (P.parseProgram) "./app/resources/code.yamil" content
        print result

        interpreter <- (CT.writePascalFile) "./app/resources/code.pas" exampleVars exampleLiterals exampleLetStatements exampleConditionExpr
        print interpreter
