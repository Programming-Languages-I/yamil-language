module Lib
        ( someFunc
        ) where

import           Parser.ParserProgram as P
import           Interpreter.CodeTranspiler as CT

import           Text.Parsec

someFunc :: IO ()
someFunc = do
        content <- readFile "./app/resources/code.yamil"
        let parseResult = parse (P.parseProgram) "./app/resources/code.yamil" content
        print parseResult

        case parseResult of
                Left err -> do
                        putStrLn "Parsing failed:"
                Right program -> do
                        CT.writePascalFile "./app/resources/code.pas" program
                        
                        putStrLn "Operation successfully."
