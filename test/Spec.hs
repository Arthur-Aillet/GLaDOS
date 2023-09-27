import ParserSpec (parserTests)
import Test.HUnit
import Control.Monad

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [ parserTests
    ]
