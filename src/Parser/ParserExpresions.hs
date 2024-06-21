module Parser.ParserExpresions (module Parser.ParserExpresions) where

import AST
import Parser.LexerParser
import Text.Parsec ((<|>), sepBy) 
import Text.Parsec.String (Parser)
import Parser.ParserValueTypes ( parseIdentifier, parseValue, parseManyTypedIdentifier )
import Parser.ParserOperations

parseExpr :: Parser Expr
parseExpr =
    parseLambda
        <|> parseFunctionCall
        <|> parseBinaryExpr
parseParamsValues :: Parser [Value]
parseParamsValues = parseOpenParents *> whiteSpaces *> parseValue `sepBy` (whiteSpaces *> parseComma <* whiteSpaces) <* whiteSpaces <* parseCloseParents

parseFunctionCall :: Parser Expr
parseFunctionCall =
    FunctionCall <$> parseIdentifier <*> parseParamsValues

parseBinaryExpr :: Parser Expr
parseBinaryExpr = BinaryExpr <$> parseValue <*> (whiteSpaces *> parseArithmeticOperator <* whiteSpaces) <*> parseValue

parseLambda :: Parser Expr
parseLambda = LambdaExpr 
              <$> (reservedNa "lambda" *> whiteSpaces *> parseOpenParents *> parseManyTypedIdentifier <* parseCloseParents <* whiteSpaces <* reservedOps "->" <* whiteSpaces )
              <*> parseExpr
