module Parser.ParserProgram (module Parser.ParserProgram) where

import AST
import Parser.ParserFunction
import Text.Parsec
import Text.Parsec.String (Parser)
import Parser.ParserExpresions (parseExpr)

parseProgram :: Parser Program
parseProgram = Program <$> endBy parseProgramElement newline <* eof

parseProgramElement :: Parser ProgramElement
parseProgramElement =
    PEFunction <$> try parseFunction
        <|> PELetStatement <$> try parseLetStatement
        <|> PEFunctionCall <$> try parseExpr