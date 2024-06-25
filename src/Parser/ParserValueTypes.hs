module Parser.ParserValueTypes
        ( module Parser.ParserValueTypes
        ) where

import           AST

import           Parser.LexerParser

import           Text.Parsec        (char, digit, letter, many, many1, noneOf,
                                     sepBy, (<|>), try)
import           Text.Parsec.String (Parser)

-- Parse Identifier
parseIdentifier :: Parser Identifier
parseIdentifier =
  (++)
    <$> many1 letter
    <*> (parseUnderscoreLetter <|> many (letter <|> digit))

parseUnderscoreLetter :: Parser String
parseUnderscoreLetter =
  (:) <$> char '_' <*> many1 letter

-- Parse Literals
parseLiteral :: Parser Literal
parseLiteral =
  (try parseDoubleLiteral) 
    <|> (try parseIntLiteral)
    <|> parseBoolLiteral
    <|> parseStringLiteral

parseIntLiteral :: Parser Literal
parseIntLiteral = IntLiteral . read <$> many1 digit

parseDoubleLiteral :: Parser Literal
parseDoubleLiteral =
  DoubleLiteral . read
    <$> ((++) <$> many1 digit <*> parseDoubleDecimalPart)

parseDoubleDecimalPart :: Parser String
parseDoubleDecimalPart = (:) <$> parseDot <*> many1 digit

parseBoolLiteral :: Parser Literal
parseBoolLiteral = BoolLiteral <$> parseBoolValues

parseStringLiteral :: Parser Literal
parseStringLiteral =
  StringLiteral
    <$> (parseQuotationMarks *> many (noneOf "\"") <* parseQuotationMarks)

-- Parse Values
parseValue :: Parser Value
parseValue =
  VLiteral <$> parseLiteral
    <|> VIdentifier <$> parseIdentifier

parseParamsValues :: Parser [Value]
parseParamsValues =
  parseOpenParents
    *> whiteSpaces
    *> parseValue `sepBy` (whiteSpaces *> parseComma <* whiteSpaces)
    <* whiteSpaces
    <* parseCloseParents

-- Parse Typed Identifier
parseTypedIdentifier :: Parser TypedIdentifier
parseTypedIdentifier =
  TypedIdentifier
    <$> (parseIdentifier <* whiteSpaces)
    <*> parseType

parseManyTypedIdentifier :: Parser [TypedIdentifier]
parseManyTypedIdentifier = many1 parseTypedIdentifier

parseType :: Parser Type
parseType =
  parseColon
    *> whiteSpaces
    *> parseSingleType

parseSingleType :: Parser Type
parseSingleType = parseTypeString
           <|> parseTypeBool
           <|> parseTypeDouble
           <|> parseTypeInt

parseTypeString :: Parser Type
parseTypeString = TString <$ parseStringSym

parseTypeBool :: Parser Type
parseTypeBool = TBool <$ parseBoolSym

parseTypeDouble :: Parser Type
parseTypeDouble = TDouble <$ parseDoubleSym

parseTypeInt :: Parser Type
parseTypeInt = TInt <$ parseIntSym
