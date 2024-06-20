module Parser.ParserPatternMatching where

import AST as AST
import Parser.ParserValueTypes
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), try) 

parsePattern :: Parser Pattern
parsePattern = (try parsePatternLiteral) <|> parsePatternIdentifier

parsePatternLiteral :: Parser Pattern
parsePatternLiteral = AST.PLiteral <$> parseLiteral 

parsePatternIdentifier :: Parser Pattern
parsePatternIdentifier = AST.PIdentifier <$> parseIdentifier
