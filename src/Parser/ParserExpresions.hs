module Parser.ParserExpresions (module Parser.ParserExpresions) where

import AST
import Parser.LexerParser
import Text.Parsec ((<|>), sepBy) 
import Text.Parsec.String (Parser)
import Parser.ParserValueTypes ( parseIdentifier, parseValue )
import Parser.ParserOperations

parseExpr :: Parser Expr
parseExpr =
    parseFunctionCall
        <|> parseBinaryExpr
parseParamsValues :: Parser [Value]
parseParamsValues = parseOpenParents *> whiteSpaces *> parseValue `sepBy` (whiteSpaces *> parseComma <* whiteSpaces) <* whiteSpaces <* parseCloseParents

parseFunctionCall :: Parser Expr
parseFunctionCall =
    FunctionCall <$> parseIdentifier <*> parseParamsValues

parseBinaryExpr :: Parser Expr
parseBinaryExpr = BinaryExpr <$> parseValue <*> (whiteSpaces *> parseArithmeticOperator <* whiteSpaces) <*> parseValue


