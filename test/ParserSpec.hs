module ParserSpec
  ( parserTests,
  )
where

import Lib
import Test.HUnit

testFoo :: Test
testFoo = TestCase $ assertEqual "Should return 2" 2 (foo 1)

parserTests :: Test
parserTests = TestList [testFoo]
