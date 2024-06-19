import Test.Hspec
import Parser.TestParserValueTypes
import Parser.TestParserOperations

main :: IO ()
main = hspec $ do
  testParseValueTypes
  testParseOperations
