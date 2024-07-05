import           Parser.TestParserConditionExpr
import           Parser.TestParserExpresions
import           Parser.TestParserFunction
import           Parser.TestParserOperations
import           Parser.TestParserPatternMatching
import           Parser.TestParserProgram
import           Parser.TestSkipComments
import           Parser.TestParserThenExpr
import           Parser.TestParserValueTypes

import           Test.Hspec
import           TestSymbolTable
import           TestSemanticAnalyzer

main :: IO ()
main = hspec $ do
  -- Parser tests
  -- testParseValueTypes
  -- testParseOperations
  -- testParserExpresions
  -- testParseLetStatement
  -- testParseLambda
  -- testParserIfExpression
  -- testParseThenExpression
  -- testParserFunctions
  -- testParserProgram
  -- testParsePatternMatches
  testSkipComments
  -- Semantic tests
  -- testSymbolTable
  -- testSemanticAnalyzer
