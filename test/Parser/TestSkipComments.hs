module Parser.TestSkipComments (module Parser.TestSkipComments) where

import Parser.LexerParser
import Test.Hspec
import Text.Parsec

-- testSkipCommentsLine :: Spec
-- testSkipCommentsLine = describe "skipComments" $ do
--     it "skips single-line comment ending with newline" $ do
--         let input = "// This is a comment\n"
--         parse skipComments input

-- testSkipComments :: Spec
-- testSkipComments = do
--     testSkipCommentsLine
