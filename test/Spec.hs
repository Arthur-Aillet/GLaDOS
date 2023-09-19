import Test.HUnit
import ParserSpec (parserTests)

main :: IO ()
main = runTestTT tests >> pure ()

tests :: Test
tests = TestList [
    parserTests
    ]