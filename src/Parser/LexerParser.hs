module Parser.LexerParser (module Parser.LexerParser) where

import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec 


languageDef :: LanguageDef st
languageDef = emptyDef
    { reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=", "(", ")", "->"]
    , reservedNames = ["if", "then", "else", "true", "false", "int", "bool", "double", "string", "lambda", "let"]
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

parseAssignSymbol :: Parser Char
parseAssignSymbol = char '='

parseDot :: Parser Char
parseDot = char '.'

parseColon :: Parser Char
parseColon = char ':'

parseUnderscore :: Parser Char
parseUnderscore = char '_'

parseQuotationMarks :: Parser Char
parseQuotationMarks = char '"'

parseIntSym :: Parser String
parseIntSym = string "int"

parseBoolSym :: Parser String
parseBoolSym = string "bool" 

parseDoubleSym :: Parser String
parseDoubleSym = string "double" 

parseStringSym :: Parser String
parseStringSym = string "string" 

parseBoolValues :: Parser Bool
parseBoolValues = (True <$ string "True") <|> (False <$ string "False")

parseOpenParents :: Parser Char
parseOpenParents = char '('

parseCloseParents :: Parser Char
parseCloseParents = char ')'

parseComma :: Parser Char
parseComma = char ','
