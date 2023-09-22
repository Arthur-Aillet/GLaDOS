module ParserSpec (
  parserTests
  , removeLastTests
) where 

import Lib
import Parser
import Test.HUnit
    
testFoo :: Test
testFoo = TestCase $ assertEqual "Should return 2" 2 (foo 1)

parserTests :: Test
parserTests = TestList [testFoo]

removeLastTests :: Test
removeLastTests = "remove last element of [String]: " ~: assertEqual "Should return [\"hello\"]" (removeLast ["hello", "world"]) ["hello"]
