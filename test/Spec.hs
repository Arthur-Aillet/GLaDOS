import Control.Monad
import ParserSpec (parserTests)
import AstSpec (parserASTTests)
import Test.HUnit

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [ parserTests,
      parserASTTests
    ]
