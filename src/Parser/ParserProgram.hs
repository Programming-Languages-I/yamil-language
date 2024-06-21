module Parser.ParserProgram
        ( module Parser.ParserProgram
        ) where

import           AST

import           Parser.LexerParser      (whiteSpaces)
import           Parser.ParserExpresions (parseExpr)
import           Parser.ParserFunction

import           Text.Parsec
import           Text.Parsec.String      (Parser)

parseProgram :: Parser Program
parseProgram = Program <$> many (whiteSpaces*> parseProgramElement <*whiteSpaces) <* eof

parseProgramElement :: Parser ProgramElement
parseProgramElement =
    PEFunction <$> try parseFunction
        <|> PELetStatement <$> try parseLetStatement
        <|> PEFunctionCall <$> try parseExpr
