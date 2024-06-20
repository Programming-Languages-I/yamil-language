module Parser.ParserExpresions (module Parser.ParserExpresions) where

import AST
import Parser.LexerParser
import Text.Parsec ((<|>), sepBy) 
import Text.Parsec.String (Parser)
import Parser.ParserValueTypes ( parseIdentifier, parseValue, parseTypedIdentifier, parseManyTypedIdentifier )
import Parser.ParserOperations

parseLetStatement :: Parser LetStatement
parseLetStatement = LetStatement <$>
         (reservedNa "let" *> whiteSpaces *> parseTypedIdentifier) <*> 
         (whiteSpaces *> reservedOps "=" *> whiteSpaces *> parseExpr)

parseExpr :: Parser Expr
parseExpr =
    parseFunctionCall
        <|> parseBinaryExpr
        <|> parseLambda
parseParamsValues :: Parser [Value]
parseParamsValues = parseOpenParents *> whiteSpaces *> parseValue `sepBy` (whiteSpaces *> parseComma <* whiteSpaces) <* whiteSpaces <* parseCloseParents

parseFunctionCall :: Parser Expr
parseFunctionCall =
    FunctionCall <$> parseIdentifier <*> parseParamsValues

parseBinaryExpr :: Parser Expr
parseBinaryExpr = BinaryExpr <$> parseValue <*> (whiteSpaces *> parseArithmeticOperator <* whiteSpaces) <*> parseValue

parseLambda :: Parser Expr
parseLambda = LambdaExpr 
              <$> (parseLambdaSymbol *> whiteSpaces *> parseOpenParents *> parseManyTypedIdentifier <* parseCloseParents <* whiteSpaces)
              <*> parseLetStatement

