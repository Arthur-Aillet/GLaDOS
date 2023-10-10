import Control.Monad
import AstSpec (parserASTTests)
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
