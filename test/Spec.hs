import Test.Hspec
import Parser.TestParserValueTypes

main :: IO ()
main = hspec $ do
  testParseValueTypes
