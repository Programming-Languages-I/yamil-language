module Parser.ParserFunction (module Parser.ParserFunction) where

import AST
import Parser.LexerParser
import Parser.ParserValueTypes
import Parser.ParserExpresions
import Text.Parsec.String (Parser)
import Text.Parsec 

parseLetStatement :: Parser LetStatement
parseLetStatement = LetStatement <$>
         (reservedNa "let" *> whiteSpaces *> parseTypedIdentifier) <*> 
         (whiteSpaces *> reservedOps "=" *> whiteSpaces *> parseExpr)
         
parseFunction :: Parser Function
parseFunction =
  Function
    <$> (reservedNa "def" *> parseIdentifier)
    <*> parseFunctionParams
    <*> parseFunctionType
    <*> (parseOpenBraces *> parseFunctionBody <* parseCloseBraces)

parseFunctionType :: Parser Type
parseFunctionType = whiteSpaces *> string "->" *> parseType

parseFunctionParams :: Parser [TypedIdentifier]
parseFunctionParams =
  parseOpenParents
    *> whiteSpaces
    *> parseTypedIdentifier `sepBy` (whiteSpaces *> parseComma <* whiteSpaces)
    <* whiteSpaces
    <* parseCloseParents

parseFunctionBody :: Parser FunctionBody
parseFunctionBody = FBody <$> (parseFunctionBodyOpts `sepEndBy` parseLineBreak)

parseFunctionBodyOpts :: Parser FunctionBodyOpts
parseFunctionBodyOpts =
  FBExpr <$> parseExpr
    <|> FBLetStatement <$> parseLetStatement

-- <|> FBPatternMatch <$> pasePatterMatch (TODO: Implement Pattern matching here)
