module Parser.ParserExpresions (module Parser.ParserExpresions) where

import AST
import Parser.LexerParser
import Text.Parsec ((<|>)) 
import Text.Parsec.String (Parser)
import Parser.ParserValueTypes ( parseIdentifier, parseValue, parseManyTypedIdentifier, parseParamsValues, parseLiteral )
import Parser.ParserOperations
import Parser.ParserConditionExpr (parseConditionExpr)

parseExpr :: Parser Expr
parseExpr =
    parseFunctionCall
        <|> parseBinaryExpr
        <|> parseIfExpr

parseFunctionCall :: Parser Expr
parseFunctionCall =
    FunctionCall <$> parseIdentifier <*> parseParamsValues

parseBinaryExpr :: Parser Expr
parseBinaryExpr = BinaryExpr <$> parseValue <*> (whiteSpaces *> parseArithmeticOperator <* whiteSpaces) <*> parseValue

parseLambda :: Parser LambdaExpr
parseLambda = LambdaExpr 
              <$> (reservedNa "lambda" *> whiteSpaces *> parseOpenParents *> parseManyTypedIdentifier <* parseCloseParents <* whiteSpaces <* reservedOps "->" <* whiteSpaces )
              <*> parseExpr

parseIfExpr :: Parser Expr
parseIfExpr =
    IfExpr
        <$> (reservedNa "if" *> whiteSpaces *> parseConditionExpr)
        <*> (whiteSpaces *> reservedNa "then" *>  whiteSpaces *> parseThenExpr)
        <*> (whiteSpaces *> reservedNa "else" *>  whiteSpaces *> parseThenExpr)

-- Parser ThenExpr
parseThenExpr :: Parser ThenExpr
parseThenExpr = 
    parseThenMainExpr
    <|> parseThenLiteral
    <|> parseThenIdentifier

parseThenLiteral :: Parser ThenExpr
parseThenLiteral = ThenLiteral <$> parseLiteral

parseThenIdentifier :: Parser ThenExpr
parseThenIdentifier = ThenIdentifier <$> parseIdentifier

parseThenMainExpr :: Parser ThenExpr
parseThenMainExpr = ThenMainExpr <$>  (parseExpr <* whiteSpaces) <*> parseExpr
