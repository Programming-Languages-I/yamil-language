module Parser where

import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token


languageDef :: LanguageDef st
languageDef = emptyDef
    { reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="]
    , reservedNames = ["if", "then", "else", "true", "false", "int", "bool", "double", "string", "lambda"]
    }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

whiteSpaces :: Parser ()
whiteSpaces = Text.Parsec.Token.whiteSpace lexer

reservedOps :: String -> Parser ()
reservedOps = Text.Parsec.Token.reservedOp lexer

reservedNa :: String -> Parser ()
reservedNa = Text.Parsec.Token.reserved lexer

