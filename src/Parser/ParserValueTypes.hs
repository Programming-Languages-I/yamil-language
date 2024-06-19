module Parser.ParserValueTypes (module Parser.ParserValueTypes) where

import AST
import Parser.LexerParser
import Text.Parsec
import Text.Parsec.String (Parser)

-- Parse Identifier
parseIdentifier :: Parser Identifier
parseIdentifier = (:) <$> letter <*> (many letter <|> many digit <|> parseUnderscoreLetter)

parseUnderscoreLetter :: Parser String
parseUnderscoreLetter = (:) <$> parseUnderscore <*> many1 letter

-- Parse Literals
parseLiteral :: Parser Literal
parseLiteral =
  parseIntLiteral
    <|> parseDoubleLiteral
    <|> parseBoolLiteral
    <|> parseStringLiteral

parseIntLiteral :: Parser Literal
parseIntLiteral = parseIntSym *> (IntLiteral . read <$> many1 digit)

parseDoubleLiteral :: Parser Literal
parseDoubleLiteral =
  parseDoubleSym
    *> ( DoubleLiteral . read
           <$> ((++) <$> many1 digit <*> parseDoubleDecimalPart)
       )

parseDoubleDecimalPart :: Parser String
parseDoubleDecimalPart = (:) <$> parseDot <*> many1 digit

parseBoolLiteral :: Parser Literal
parseBoolLiteral = parseBoolSym *> (BoolLiteral <$> parseBoolValues)

parseStringLiteral :: Parser Literal
parseStringLiteral =
  parseStringSym
    *> ( StringLiteral
           <$> (parseQuotationMarks *> many (noneOf "\"") <* parseQuotationMarks)
       )

-- Parse Values
parseValue :: Parser Value
parseValue = VLiteral <$> parseLiteral <|> VIdentifier <$> parseIdentifier

-- TODO: Add the implementation for parseFunctionCall here

-- Parse Typed Identifier
parseTypedIdentifier :: Parser TypedIdentifier
parseTypedIdentifier =
  TypedIdentifier
    <$> (parseIdentifier <* whiteSpaces)
    <*> (whiteSpaces *> parseType)

parseType :: Parser Type
parseType =
  parseColon
    *> whiteSpaces
    *> ( parseTypeString
           <|> parseTypeBool
           <|> parseTypeDouble
           <|> parseTypeInt
       )

parseTypeString :: Parser Type
parseTypeString = TString <$ parseStringLiteral

parseTypeBool :: Parser Type
parseTypeBool = TBool <$ parseBoolLiteral

parseTypeDouble :: Parser Type
parseTypeDouble = TDouble <$ parseDoubleLiteral

parseTypeInt :: Parser Type
parseTypeInt = TInt <$ parseIntLiteral