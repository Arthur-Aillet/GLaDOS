import AstSpec (parserASTTests)
import Control.Monad
import ConverterSpec (converterTests)
import ParserSpec (parserTests)
import Test.HUnit

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [ parserTests,
      parserASTTests,
      converterTests
    ]
