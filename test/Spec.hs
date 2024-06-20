import Test.Hspec
import Parser.TestParserValueTypes
import Parser.TestParserOperations
import Parser.TestParserExpresions


main :: IO ()
main = hspec $ do
  testParseValueTypes
  testParseOperations
  testParserExpresions
  