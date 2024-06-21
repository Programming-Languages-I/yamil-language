module Lib
    ( someFunc
    ) where

import Text.Parsec
import Parser.ParserProgram as P

someFunc :: IO ()
someFunc = do
    content <- readFile "./app/resources/code.yamil"
    let result = parse (P.parseProgram) "./app/resources/code.yamil" content
    print result