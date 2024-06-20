module Parser.TestParserPatternMatching (module Parser.TestParserPatternMatching) where

import AST
import Parser.ParserPatternMatching
import Test.Hspec
import Text.Parsec

testParsePatternMatching :: Spec
testParsePatternMatching = describe "PatternMatchParser" $ do
    it "parses a single match case" $ do
        parse parsePatternMatches "" "0 -> \"Zero\"" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (StringLiteral "Zero"))]

testParsePatternMatches :: Spec
testParsePatternMatches = do 
  testParsePatternMatching 
