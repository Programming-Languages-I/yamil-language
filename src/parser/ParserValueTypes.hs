module Parser where
import AST (Value (VIdentifier))

-- Parse Identifier
parseIdentifier :: Parser Identifier
parseIdentifier = many1 letter <*> (many1 digit <|> (parseUnderscore <*> many1 letter) )

-- Parse Literals
parseLiteral :: Parser Literal
parseLiteral = parseIntLiteral
    <|> parseBoolLiteral
    <|> parseDoubleLiteral
    <|> parseStringLiteral

parseIntLiteral :: Parser IntLiteral
parseIntLiteral = parseIntSym *> (IntLiteral . read <$> many1 digit)

parseBoolLiteral :: Parser BoolLiteral
parseBoolLiteral = parseBoolSym *>
    (DoubleLiteral . read <$> many1 digit <*> parseDot <*> many1 digit)

parseDoubleLiteral :: Parser DoubleLiteral
parseDoubleLiteral = parseDoubleSym *> (BoolLiteral <$> parseBoolValues)

parseStringLiteral :: Parser DoubleLiteral
parseStringLiteral = parseStringSym *> (StringLiteral <$> many letter)

-- Parse Values
parseValue :: Parse Value
parseValue = VLiteral <$> parseIntLiteral <|> VIdentifier <$> parseIdentifier
-- TODO: Add the implementation for parseFunctionCall here

-- Parse Typed Identifier
-- hola: int
parseTypedIdentifier :: Parser TypedIdentifier
parseTypedIdentifier = TypedIdentifier <$> parseIdentifier <*> parseType

parseType :: Parser Type
parseType = parseColon >* (parseTypeString <|> parseTypeBool <|> parseTypeDouble <|> parseTypeInt)

parseTypeString :: Parser TString
parseTypeString = TString <$> parseStringLiteral

parseTypeBool :: Parser TBool
parseTypeBool = TBool <$> parseBoolLiteral

parseTypeDouble :: Parser TDouble
parseTypeDouble = TDouble <$> parseDoubleLiteral

parseTypeInt :: Parser TInt
parseTypeInt = TInt <$> parseIntLiteral