module Lib
    ( someFunc
    ) where

import Text.Parsec
import Parser.ParserProgram as P

someFunc :: IO ()
someFunc = do
    content <- readFile "src/resources/Example.yamil"
    let result = parse (P.parseProgram) "src/resources/Example.yamil" content
    print result