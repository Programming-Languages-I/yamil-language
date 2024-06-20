module Parser.ParserOperations (module Parser.ParserOperations)where

import AST
import Parser.LexerParser
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

parseArithmeticOperator :: Parser ArithmeticOperator
parseArithmeticOperator =
    parseAdd
        <|> parseSubtract
        <|> parseMultiply
        <|> parseDivide

parseAdd :: Parser ArithmeticOperator
parseAdd = Add <$ reservedOps "+"

parseSubtract :: Parser ArithmeticOperator
parseSubtract = Subtract <$ reservedOps "-"

parseMultiply :: Parser ArithmeticOperator
parseMultiply = Multiply <$ reservedOps "*"

parseDivide :: Parser ArithmeticOperator
parseDivide = Divide <$ reservedOps "/"
