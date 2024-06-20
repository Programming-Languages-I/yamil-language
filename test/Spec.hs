import Test.Hspec
import Parser.TestParserValueTypes
import Parser.TestParserOperations
import Parser.TestParserExpresions
import Parser.TestParserFunction
import Parser.TestParserConditionExpr
import Parser.TestParserThenExpr


main :: IO ()
main = hspec $ do
  testParseValueTypes
  testParseOperations
  testParserExpresions
  testParseLetStatement
  testParseLambda
  testParserIfExpression
  testParseThenExpression
  
