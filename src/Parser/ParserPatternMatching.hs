module Parser.ParserPatternMatching where

import           AST                     as AST

import           Parser.LexerParser
import           Parser.ParserValueTypes

import           Text.Parsec             (lookAhead, manyTill, try, (<|>))
import           Text.Parsec.String      (Parser)

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

parsePatternMatches :: Parser PatternMatches
parsePatternMatches = AST.FullPatternMatch <$> (manyTill (parsePatternMatchUsingGuards <* whiteSpaces <* reservedOps "|" <* whiteSpaces) (try (lookAhead (reservedNa "otherwise")))) <*> parseOtherwiseMatchLit
