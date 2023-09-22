import Test.HUnit
import ParserSpec (parserTests, removeLastTests)

main :: IO ()
main = runTestTT tests >> pure ()

tests :: Test
tests = TestList [
    parserTests
    , removeLastTests
    ]
