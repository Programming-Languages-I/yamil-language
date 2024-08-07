module Parser.ParserFunction
        ( module Parser.ParserFunction
        ) where

import           AST

import           Parser.LexerParser
import           Parser.ParserExpresions
import           Parser.ParserPatternMatching
import           Parser.ParserValueTypes

import           Text.Parsec
import           Text.Parsec.String           (Parser)
import           Text.Parsec.Token            (GenTokenParser (whiteSpace))

parseLetStatement :: Parser LetStatement
parseLetStatement =
  LetStatement
    <$> (whiteSpaces *> reservedNa "let" *> whiteSpaces *> parseTypedIdentifier)
    <*> (whiteSpaces *> reservedOps "=" *> whiteSpaces *> parseExpr)

parseFunction :: Parser Function
parseFunction =
  Function
    <$> (parseFunctionSym *> parseIdentifier)
    <*> parseFunctionParams
    <*> parseFunctionType
    <*> ( parseOpenBraces
            *> whiteSpaces
            *> parseFunctionBody
            <* whiteSpaces
            <* parseCloseBraces
        )

parseFunctionType :: Parser Type
parseFunctionType =
  whiteSpaces
    *> string "->"
    *> whiteSpaces
    *> parseSingleType
    <* whiteSpaces
    <* parseAssignSymbol
    <* whiteSpaces

parseFunctionParams :: Parser [TypedIdentifier]
parseFunctionParams =
  parseOpenParents
    *> whiteSpaces
    *> parseTypedIdentifier `sepBy` (whiteSpaces *> parseComma <* whiteSpaces)
    <* whiteSpaces
    <* parseCloseParents

parseFunctionBody :: Parser FunctionBody
parseFunctionBody =
  FBLambdaExpr <$> try parseLambda <*> try parseFunctionBodyOpts `sepEndBy` parseLineBreak
  <|> FBPatternMatch <$> try (whiteSpaces *> parsePatternMatches <* whiteSpaces)
  <|> FBody <$> (parseFunctionBodyOpts `sepEndBy` parseLineBreak)

parseFunctionBodyOpts :: Parser FunctionBodyOpts
parseFunctionBodyOpts =
  FBLetStatement <$> try parseLetStatement
    <|> FBExpr <$> try (whiteSpaces *> parseExpr)
    <|> FBEmpty <$ (whiteSpaces)
