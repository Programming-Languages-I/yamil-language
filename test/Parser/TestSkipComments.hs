module Parser.TestSkipComments
        ( module Parser.TestSkipComments
        ) where

import           Parser.LexerParser

import           Test.Hspec

import           Text.Parsec

testSkipCommentsLine :: Spec
testSkipCommentsLine = describe "skipComments" $ do
  it "skips single-line comment ending with newline" $ do
    let input = "// This is a comment\n"
    let result = parse skipComments "" input
    result `shouldBe` Right ()

testSkipComments :: Spec
testSkipComments = do
  testSkipCommentsLine
