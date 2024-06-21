module Lib
        ( someFunc
        ) where

import           Parser.ParserProgram as P

import           Text.Parsec

someFunc :: IO ()
someFunc = do
    content <- readFile "./app/resources/code.yamil"
    let result = parse (P.parseProgram) "./app/resources/code.yamil" content
    print result
