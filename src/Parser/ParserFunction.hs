module Parser.ParserFunction (module Parser.ParserFunction) where

import AST
import Parser.LexerParser
import Parser.ParserValueTypes ( parseTypedIdentifier )
import Parser.ParserExpresions
import Text.Parsec.String (Parser)

parseLetStatement :: Parser LetStatement
parseLetStatement = LetStatement <$>
         (reservedNa "let" *> whiteSpaces *> parseTypedIdentifier) <*> 
         (whiteSpaces *> reservedOps "=" *> whiteSpaces *> parseExpr)
