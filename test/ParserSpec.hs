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
removeLastTests = "Remove last element of [String]" ~: do
    assertEqual "Empty array: " (removeLast []) []
    assertEqual "Array of 1 element: " (removeLast [")"]) []
    assertEqual "Array of 2 elements: " (removeLast ["hello", ")"]) ["hello"]
    assertEqual "Array of 3 elements: " (removeLast ["hello", "world", ")"]) ["hello", "world"]
