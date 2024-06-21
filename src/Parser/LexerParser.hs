module Parser.LexerParser (module Parser.LexerParser) where

import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec 


languageDef :: LanguageDef st
languageDef = emptyDef
    { reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=", "(", ")", "->", "="]
    , reservedNames = ["if", "then", "else", "True", "False", "int", "bool", "double", "string", "lambda", "let", "def"]
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

parseAssignSymbol :: Parser()
parseAssignSymbol = reservedOps "="

parseDot :: Parser Char
parseDot = char '.'

parseColon :: Parser Char
parseColon = char ':'

parseUnderscore :: Parser Char
parseUnderscore = char '_'

parseQuotationMarks :: Parser Char
parseQuotationMarks = char '"'

parseIntSym :: Parser()
parseIntSym = reservedNa "int"

parseBoolSym :: Parser()
parseBoolSym = reservedNa "bool" 

parseDoubleSym :: Parser()
parseDoubleSym = reservedNa "double" 

parseStringSym :: Parser()
parseStringSym = reservedNa "string" 

parseBoolValues :: Parser Bool
parseBoolValues = (True <$ reservedNa "True") <|> (False <$ reservedNa "False")

parseOpenParents :: Parser Char
parseOpenParents = char '('

parseCloseParents :: Parser Char
parseCloseParents = char ')'

parseOpenBraces :: Parser Char
parseOpenBraces = char '{'

parseCloseBraces :: Parser Char
parseCloseBraces = char '}'

parseComma :: Parser Char
parseComma = char ','

parseLineBreak :: Parser Char
parseLineBreak = char '\n' <|> char '\r'

parseFunctionSym :: Parser ()
parseFunctionSym = reservedNa "def"
