module Parser.ParserProgram (module Parser.ParserProgram) where

import AST
import Parser.ParserFunction
import Text.Parsec
import Text.Parsec.String (Parser)
import Parser.ParserExpresions (parseExpr)
import Parser.LexerParser (whiteSpaces)

parseProgram :: Parser Program
parseProgram = Program <$> many (whiteSpaces*> parseProgramElement <*whiteSpaces) <* eof

parseProgramElement :: Parser ProgramElement
parseProgramElement =
    PEFunction <$> try parseFunction
        <|> PELetStatement <$> try parseLetStatement
        <|> PEFunctionCall <$> try parseExpr