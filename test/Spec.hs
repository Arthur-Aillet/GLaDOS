import ParserSpec (parserTests)
import Test.HUnit

main :: IO ()
main = runTestTT tests >> pure ()

tests :: Test
tests =
  TestList
    [ parserTests
    ]
