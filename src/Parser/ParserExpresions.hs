module Parser.ParserExpresions (module Parser.ParserExpresions) where

import AST
import Parser.LexerParser
import Text.Parsec ((<|>)) 
import Text.Parsec.String (Parser)
import Parser.ParserValueTypes ( parseIdentifier, parseValue, parseManyTypedIdentifier, parseParamsValues )
import Parser.ParserOperations

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
