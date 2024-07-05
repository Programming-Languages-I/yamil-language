module Lib
        ( someFunc
        ) where

import qualified Data.Map                   as Map

import           Interpreter.CodeTranspiler as CT

import           Parser.ParserProgram       as P

import           Semantic.SemanticAnalyzer

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

        -- case parseResult of
        --         Left parseError -> print parseError
        --         Right program -> do
        --         let result2 = analyzeProgram program Map.empty
        --         print result2
