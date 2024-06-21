module Parser.ParserThenExpr (module Parser.ParserThenExpr) where

import AST ( ThenExpr(..) )
import Parser.LexerParser (whiteSpaces)
import Parser.ParserValueTypes (parseLiteral, parseIdentifier)
import Parser.ParserExpresions (parseExpr)
import Text.Parsec ((<|>) ,)
import Text.Parsec.String (Parser)

-- Parser para ThenExpr
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
