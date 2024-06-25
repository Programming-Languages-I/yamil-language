module Parser.ParserPatternMatching where

import AST as AST
import Parser.ParserValueTypes 
import Parser.LexerParser
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), try) 

parsePattern :: Parser Pattern
parsePattern = (try parsePatternLiteral) <|> parsePatternIdentifier

parsePatternLiteral :: Parser Pattern
parsePatternLiteral = AST.PLiteral <$> (try parseLiteral)

parsePatternIdentifier :: Parser Pattern
parsePatternIdentifier = AST.PIdentifier <$> (try parseIdentifier)

parsePatternMatchUsingGuards :: Parser PatternMatch
parsePatternMatchUsingGuards = (try parsePatternMatchLit)

parsePatternMatchLit :: Parser PatternMatch
parsePatternMatchLit = AST.PatternMatchLit <$> (whiteSpaces *> parsePattern) <*> (whiteSpaces *> reservedOps "->" *> whiteSpaces *> parseLiteral)

parseOtherwiseMatchLit :: Parser OtherwiseMatch
parseOtherwiseMatchLit = AST.OtherwiseLit <$> (reservedNa "otherwise" *> reservedOps "->" *> parseLiteral)
