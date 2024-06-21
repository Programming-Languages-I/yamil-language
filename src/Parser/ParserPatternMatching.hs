module Parser.ParserPatternMatching where

import AST as AST
import Parser.ParserValueTypes 
import Parser.ParserExpresions
import Parser.LexerParser
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), sepBy, try) 

parsePattern :: Parser Pattern
parsePattern = (try parsePatternLiteral) <|> parsePatternIdentifier

parsePatternLiteral :: Parser Pattern
parsePatternLiteral = AST.PLiteral <$> (try parseLiteral)

parsePatternIdentifier :: Parser Pattern
parsePatternIdentifier = AST.PIdentifier <$> (try parseIdentifier)

parsePatternMatches :: Parser [PatternMatch]
parsePatternMatches = parsePatternMatchUsingGuards `sepBy` reservedOps "|"

parsePatternMatchUsingGuards :: Parser PatternMatch
-- parsePatternMatchUsingGuards = (try parsePatternMatch) <|> parseOtherwise
parsePatternMatchUsingGuards = (try parsePatternMatchLit)

parsePatternMatchLit :: Parser PatternMatch
parsePatternMatchLit = AST.PatternMatchLit <$> parsePattern <* (whiteSpaces *> (reservedOps "->") <* whiteSpaces) <*> (parseLiteral)

-- parseOtherwise :: Parser PatternMatch
-- parseOtherwise = AST.Otherwise <$> (reservedNa "otherwise") <* (whiteSpaces *> (reservedOps "->") <* whiteSpaces) <*> (parseLiteral)
