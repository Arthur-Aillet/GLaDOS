module ParserSpec (
  parserTests  
) where 

import Test.HUnit

foo::Int ->Int
foo = (+1)
    
testFoo :: Test
testFoo = TestCase $ assertEqual "Should return 2" 2 (foo 1)

parserTests :: Test
parserTests = TestList [testFoo]