module Parser.ParserConditionExpr (module Parser.ParserConditionExpr) where

import AST 
import Parser.LexerParser(whiteSpaces , reservedOps , parseBoolValues , whiteSpaces ,reservedNa)
import Parser.ParserValueTypes (parseValue)
import Text.Parsec ((<|>) )
import Text.Parsec.String (Parser)

parseConditionExpr :: Parser ConditionExpr
parseConditionExpr =
    parseCondition
    <|> parseConditionOr
    <|> parseConditionAnd
    <|> parseConditionBool

parseCondition :: Parser ConditionExpr
parseCondition = Condition <$> parseValue <*> (whiteSpaces *> parseComparisonOperator <* whiteSpaces) <*> parseValue

parseConditionAnd :: Parser ConditionExpr
parseConditionAnd = ConditionAnd <$> (parseConditionExpr <* whiteSpaces <* reservedOps "&&" <* whiteSpaces) <*> parseConditionExpr

parseConditionOr :: Parser ConditionExpr
parseConditionOr = ConditionOr <$> (parseConditionExpr <* whiteSpaces <* reservedOps "||" <* whiteSpaces) <*> parseConditionExpr

parseConditionBool :: Parser ConditionExpr
parseConditionBool = ConditionBool <$> parseBoolValues

parseComparisonOperator :: Parser ComparisonOperator
parseComparisonOperator =
    parseLessThan
    <|> parseGreaterThan
    <|> parseLessEqual
    <|> parseGreaterEqual
    <|> parseEqual
    <|> parseNotEqual


parseLessThan :: Parser ComparisonOperator
parseLessThan = LessThan <$ reservedOps "<"

parseGreaterThan :: Parser ComparisonOperator
parseGreaterThan = GreaterThan <$ reservedOps ">"

parseLessEqual :: Parser ComparisonOperator
parseLessEqual = LessEqual <$ reservedOps "<="

parseGreaterEqual :: Parser ComparisonOperator
parseGreaterEqual = GreaterEqual <$ reservedOps ">="

parseEqual :: Parser ComparisonOperator
parseEqual = Equal <$ reservedOps "=="

parseNotEqual :: Parser ComparisonOperator
parseNotEqual = NotEqual <$ reservedOps "!="


