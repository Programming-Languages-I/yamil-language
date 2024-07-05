module Parser.ParserExpresions
        ( module Parser.ParserExpresions
        ) where

import           AST

import           Parser.LexerParser
import           Parser.ParserConditionExpr (parseConditionExpr)
import           Parser.ParserOperations
import           Parser.ParserValueTypes    (parseIdentifier, parseLiteral,
                                             parseManyTypedIdentifier,
                                             parseParamsValues, parseValue)

import           Text.Parsec
import           Text.Parsec.String         (Parser)

parseExpr :: Parser Expr
parseExpr =
    try parseBinaryExpr
        <|> try parseFunctionCall
        <|> try parseIfExpr
        <|> (ValueExpr <$> parseValue)

parseFunctionCall :: Parser Expr
parseFunctionCall =
    FunctionCall <$> parseIdentifier <*> parseParamsValues

parseBinaryExpr :: Parser Expr
parseBinaryExpr = BinaryExpr <$> parseValue <*> (whiteSpaces *> parseArithmeticOperator <* whiteSpaces) <*> parseValue

parseLambda :: Parser LambdaExpr
parseLambda = LambdaExpr
              <$> (reservedNa "lambda" *> whiteSpaces *> parseIdentifier <* whiteSpaces)
              <*> (parseOpenParents *> parseManyTypedIdentifier <* parseCloseParents <* whiteSpaces)
              <*> (reservedOps "->" *> whiteSpaces *> parseExpr)

parseIfExpr :: Parser Expr
parseIfExpr =
    IfExpr
        <$> (reservedNa "if" *> whiteSpaces *> parseConditionExpr)
        <*> (whiteSpaces *> reservedNa "then" *>  whiteSpaces *> parseThenExpr)
        <*> (whiteSpaces *> reservedNa "else" *>  whiteSpaces *> parseThenExpr)

-- Parser ThenExpr
parseThenExpr :: Parser ThenExpr
parseThenExpr =
    try parseThenMainExpr
    <|> try parseThenLiteral
    <|> try parseThenIdentifier

parseThenLiteral :: Parser ThenExpr
parseThenLiteral = ThenLiteral <$> parseLiteral

parseThenIdentifier :: Parser ThenExpr
parseThenIdentifier = ThenIdentifier <$> parseIdentifier

parseThenMainExpr :: Parser ThenExpr
parseThenMainExpr = ThenMainExpr <$>  (parseExpr <* whiteSpaces)
