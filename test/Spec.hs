import Lib
import Test.HUnit

addPositivesTest :: Test
addPositivesTest = TestCase (assertEqual "for (add 1 2)," 3 (add 1 2))

addNegativesTest :: Test
addNegativesTest = TestCase (assertEqual "for (add (-1) (-2))," (-3) (add (-1) (-2)))

addPositiveAndNegativeTest :: Test
addPositiveAndNegativeTest = TestCase (assertEqual "for (add 1 (-2))," (-1) (add 1 (-2)))

addWithZeroTest :: Test
addWithZeroTest = TestCase (assertEqual "for (add 0 5)," 5 (add 0 5))

tests :: Test
tests = TestList [
          TestLabel "addPositives" addPositivesTest,
          TestLabel "addNegativesTest" addNegativesTest,
          TestLabel "addPositiveAndNegativeTest" addPositiveAndNegativeTest,
          TestLabel "addWithZeroTest" addWithZeroTest
        ]

main :: IO ()
main = runTestTT tests >>= print
