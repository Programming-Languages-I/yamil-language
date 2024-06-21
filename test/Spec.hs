import Test.Hspec
import Parser.TestParserValueTypes
import Parser.TestParserOperations
import Parser.TestParserExpresions
import Parser.TestParserFunction

main :: IO ()
main = hspec $ do
  testParseValueTypes
  testParseOperations
  testParserExpresions
  testParseLetStatement
  testParseLambda
  
