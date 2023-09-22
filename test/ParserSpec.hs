module ParserSpec (
  parserTests
) where 

import Lib
import Parser
import Test.HUnit
    
testFoo :: Test
testFoo = TestCase $ assertEqual "Should return 2" 2 (foo 1)

parserTests :: Test
parserTests = TestList
          [ testFoo
          , removeLastTests
          , removeFirstAndLastTests
          ]

removeLastTests :: Test
removeLastTests = TestList
  [ "removeLast: Empty array: " ~: assertEqual "Should return []" (removeLast []) []
  , "removeLast: Array of 1 element: " ~: assertEqual "Should return []" (removeLast [")"]) []
  , "removeLast: Array of 2 element: " ~: assertEqual "Should return [\"hello\"]" (removeLast ["hello", ")"]) ["hello"]
  , "removeLast: Array of 3 element: " ~: assertEqual "Should return [\"hello\", \"world\"]" (removeLast ["hello", "world", ")"]) ["hello", "world"]
  ]

removeFirstAndLastTests :: Test
removeFirstAndLastTests = TestList
  [ "removeFirstAndLast: Empty array: " ~: assertEqual "Should return []" (removeFirstAndLast []) []
  , "removeFirstAndLast: Array of 1 element: " ~: assertEqual "Should return []" (removeFirstAndLast ["x"]) []
  , "removeFirstAndLast: Array of 2 element: " ~: assertEqual "Should return []" (removeFirstAndLast ["(", ")"]) []
  , "removeFirstAndLast: Array of 3 element: " ~: assertEqual "Should return [\"hello\"]" (removeFirstAndLast ["(", "hello", ")"]) ["hello"]
  ]

