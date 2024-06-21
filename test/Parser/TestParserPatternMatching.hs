module Parser.TestParserPatternMatching (module Parser.TestParserPatternMatching) where

import AST
import Parser.ParserPatternMatching
import Test.Hspec
import Text.Parsec

testParsePatternMatchingIntLiteral :: Spec
testParsePatternMatchingIntLiteral = describe "parsePatternMatchesIntLiteral" $ do
    it "parses an Integer -> Integer match case" $ do
        parse parsePatternMatches "" "0 -> 0" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (IntLiteral 0))]

    it "parses an Integer -> Bool match case" $ do
        parse parsePatternMatches "" "0 -> True" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (BoolLiteral True))] 

    -- it "parses an Integer -> Double match case" $ do
    --     parse parsePatternMatches "" "0 -> 1.0" `shouldBe` 
    --                     Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (DoubleLiteral 1.0))]

    it "parses an Integer -> String match case" $ do
        parse parsePatternMatches "" "0 -> \"Zero\"" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (StringLiteral "Zero"))]

testParsePatternMatchingBoolLiteral :: Spec
testParsePatternMatchingBoolLiteral = describe "parsePatternMatchesBoolLiteral" $ do
    it "parses a Bool -> Integer match case" $ do
        parse parsePatternMatches "" "False -> 17" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (BoolLiteral False)) (IntLiteral 17))]

    it "parses a Bool -> Bool match case" $ do
        parse parsePatternMatches "" "False -> False" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (BoolLiteral False)) (BoolLiteral False))]                        

    -- it "parses a Bool -> Double match case" $ do
    --     parse parsePatternMatches "" "True -> 17.0123" `shouldBe` 
    --                     Right [(PatternMatchLit (PLiteral (BoolLiteral True)) (DoubleLiteral 17.0123))]                        

    it "parses a Bool -> String match case" $ do
        parse parsePatternMatches "" "True -> \"True\"" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (BoolLiteral True)) (StringLiteral "True"))]

-- testParsePatternMatchingDoubleLiteral :: Spec
-- testParsePatternMatchingDoubleLiteral = describe "parsePatternMatchesDoubleLiteral" $ do
--     it "parses a Double -> Integer match case" $ do
--         parse parsePatternMatches "" "123.4 -> 123" `shouldBe` 
--                         Right [(PatternMatchLit (PLiteral (DoubleLiteral 123.4)) (IntLiteral 123))]

--     it "parses a Double -> Bool match case" $ do
--         parse parsePatternMatches "" "123.4 -> True" `shouldBe` 
--                         Right [(PatternMatchLit (PLiteral (DoubleLiteral 123.4)) (BoolLiteral True))]

--     it "parses a Double -> Double match case" $ do
--         parse parsePatternMatches "" "123.4 -> 123.4" `shouldBe` 
--                         Right [(PatternMatchLit (PLiteral (DoubleLiteral 123.4)) (DoubleLiteral 123.4))]

--     it "parses a Double -> String match case" $ do
--         parse parsePatternMatches "" "123.4 -> \"Dos . Cinco\"" `shouldBe` 
--                         Right [(PatternMatchLit (PLiteral (DoubleLiteral 123.4)) (StringLiteral "Dos . Cinco"))]

testParsePatternMatchingStringLiteral :: Spec
testParsePatternMatchingStringLiteral = describe "parsePatternMatchesStringLiteral" $ do
    it "parses a String -> Integer match case" $ do
        parse parsePatternMatches "" "\"Blue\" -> 2024" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (StringLiteral "Blue")) (IntLiteral 2024))]

    it "parses a String -> Bool match case" $ do
        parse parsePatternMatches "" "\"Haskell\" -> True" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (StringLiteral "Haskell")) (BoolLiteral True))]                        

    -- it "parses a String -> Double match case" $ do
    --     parse parsePatternMatches "" "\"Blue\" -> 2.5" `shouldBe` 
    --                     Right [(PatternMatchLit (PLiteral (StringLiteral "Blue")) (DoubleLiteral 2.5))]

    it "parses a String -> String match case" $ do
        parse parsePatternMatches "" "\"Blue\" -> \"Azul\"" `shouldBe` 
                        Right [(PatternMatchLit (PLiteral (StringLiteral "Blue")) (StringLiteral "Azul"))]
    
testParsePatternMatchingPIdentifier :: Spec
testParsePatternMatchingPIdentifier = describe "parsePatternMatchesPIdentifier" $ do
    it "parses an Identifier -> Integer match case" $ do
        parse parsePatternMatches "" "Zero -> 1" `shouldBe`
                        Right [(PatternMatchLit (PIdentifier "Zero")) (IntLiteral 1)]

    it "parses an Identifier -> Bool match case" $ do
        parse parsePatternMatches "" "Zero -> True" `shouldBe`
                        Right [(PatternMatchLit (PIdentifier "Zero")) (BoolLiteral True)]  

    -- it "parses an Identifier -> Double match case" $ do
    --     parse parsePatternMatches "" "Zero . Punto -> 0.1" `shouldBe`
    --                     Right [(PatternMatchLit (PIdentifier "Zero")) (DoubleLiteral 0.1)]

    it "parses an Identifier -> String match case" $ do
        parse parsePatternMatches "" "Zero -> \"Zero\"" `shouldBe`
                        Right [(PatternMatchLit (PIdentifier "Zero")) (StringLiteral "Zero")]

testParsePatternMatchingOtherwiseLit :: Spec
testParsePatternMatchingOtherwiseLit = describe "parseOtherwiseLit" $ do
    it "parses an Otherwise -> Integer match case" $ do
        parse parseOtherwiseLit "" "otherwise -> 2004" `shouldBe`
                        Right (OtherwiseLit (IntLiteral 2004))

    it "parses an Otherwise -> Bool match case" $ do
        parse parseOtherwiseLit "" "otherwise -> False" `shouldBe`
                        Right (OtherwiseLit (BoolLiteral False))

    -- it "parses an Otherwise -> Double match case" $ do
    --     parse parsePatternMatches "" "otherwise -> 2.5666" `shouldBe`
    --                     Right (OtherwiseLit (DoubleLiteral 2.5666))

    it "parses an Otherwise -> String match case" $ do
        parse parseOtherwiseLit "" "otherwise -> \"None\"" `shouldBe`
                        Right (OtherwiseLit (StringLiteral "None"))

-- testParsePatternMatchingIntLiterals :: Spec
-- testParsePatternMatchingIntLiterals = describe "parsePatternMatchesIntLiteral" $ do
--     it "parses multiple Integer -> Integer match cases separated by |" $ do
--         parse parsePatternMatches "" "0 -> 0 | 1 -> 1" `shouldBe` 
--                         Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (IntLiteral 0))
--                         , (PatternMatchLit (PLiteral (IntLiteral 1)) (IntLiteral 1))]
--     it "parses multiple Integer -> Integer match cases separated by otherwise" $ do
--         parse parsePatternMatches "" "0 -> 0 | otherwise -> 1" `shouldBe` 
--                         Right [(PatternMatchLit (PLiteral (IntLiteral 0)) (IntLiteral 0))
--                         , (OtherwiseLit (IntLiteral 1))]

    
testParsePatternMatches :: Spec
testParsePatternMatches = do 
  testParsePatternMatchingIntLiteral
  -- testParsePatternMatchingDoubleLiteral
  testParsePatternMatchingBoolLiteral
  testParsePatternMatchingStringLiteral
  testParsePatternMatchingPIdentifier
  testParsePatternMatchingOtherwiseLit
--   testParsePatternMatchingIntLiterals
