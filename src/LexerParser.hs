module LexerParser (module LexerParser) where

import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import AST
import Text.Parsec (many1, digit, string)


languageDef :: LanguageDef st
languageDef = emptyDef
    { reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="]
    , reservedNames = ["if", "then", "else", "true", "false", "int", "bool", "double", "string", "lambda"]
    , commentLine = "//"
    }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

whiteSpaces :: Parser ()
whiteSpaces = Text.Parsec.Token.whiteSpace lexer

reservedOps :: String -> Parser ()
reservedOps = Text.Parsec.Token.reservedOp lexer

reservedNa :: String -> Parser ()
reservedNa = Text.Parsec.Token.reserved lexer

parseLiteral :: Parser Literal
parseLiteral = parseInt

parseInt :: Parser Literal
parseInt =  parseIntSym *> (IntLiteral . read <$> many1 digit)

parseIntSym :: Parser String
parseIntSym = string "int" 
