module ParserSpec (
  parserTests
) where

import SParser (SExpr (SInt, SBool, SFloat, SSym, SList)
        ,sExprParser
        , strArrayToSExpr
        , readSExpr
        , sexprSplit
        , isBetween
        , removeParenthesis
        , getParenthesis
        , getInsideParentheses
        , readBool
        , separateChar
        , separateCharOnList
        , clean
        )
import Test.HUnit

parserTests :: Test
parserTests = TestList
          [ sExprParserTests
          , strArrayToSExprTests
          , readSExprTests
          , sexprSplitTests
          , isBetweenTests
          , removeParenthesisTests
          , getParenthesisTests
          , getInsideParenthesesTests
          , readBoolTests
          , separateCharTests
          , separateCharOnListTests
          , cleanTests
          ]

cleanTests :: Test
cleanTests = TestList
  [ "clean: Remove matching element" ~: assertEqual "Should return cleaned list" (clean [1, 2, 3, 2, 4 :: Int] 2) [1, 3, 4]
  , "clean: Empty list" ~: assertEqual "Should return empty list" (clean ([] :: [Int]) 2) ([] :: [Int])
  , "clean: No matching element" ~: assertEqual "Should return original list" (clean [1, 2, 3, 4, 5 :: Int] 6) [1, 2, 3, 4, 5]
  ]

separateCharOnListTests :: Test
separateCharOnListTests = TestList
  [ "separateCharOnList: Single character separation" ~: assertEqual "Should return list with separated characters" (separateCharOnList ["a(b)c(d)e"] '(') ["a","(","b)c","(","d)e"]
  , "separateCharOnList: No separation character" ~: assertEqual "Should return original list" (separateCharOnList ["abcdefg"] '(') ["abcdefg"]
  , "separateCharOnList: Empty input list" ~: assertEqual "Should return empty list" (separateCharOnList [] '(') ([] :: [String])
  ]

separateCharTests :: Test
separateCharTests = TestList
  [ "separateChar: Single character separation" ~: assertEqual "Should return list with separated characters" (separateChar "a(b)c(d)e" '(') ["a", "(", "b)c", "(" ,"d)e"]
  , "separateChar: No separation character" ~: assertEqual "Should return original list" (separateChar "abcdefg" '(') ["abcdefg"]
  , "separateChar: Empty input" ~: assertEqual "Should return empty list" (separateChar "" '(') ([""] :: [String])
  ]

readBoolTests :: Test
readBoolTests = TestList
  [ "readBool: True" ~: assertEqual "Should return Just True" (readBool "#t") (Just True)
  , "readBool: False" ~: assertEqual "Should return Just False" (readBool "#f") (Just False)
  , "readBool: Invalid input" ~: assertEqual "Should return Nothing" (readBool "not_a_bool") Nothing
  ]

sExprParserTests :: Test
sExprParserTests = TestList
  [ "sExprParser: Empty input" ~: assertEqual "Should return []" (sExprParser "") []
  , "sExprParser: Single integer" ~: assertEqual "Should return [SInt 42]" (sExprParser "42") [SInt 42]
  , "sExprParser: Nested expressions" ~: assertEqual "Should return nested SList" (sExprParser "(1 (2 3) 4)") [SList [SInt 1, SList [SInt 2, SInt 3], SInt 4]]
  ]

strArrayToSExprTests :: Test
strArrayToSExprTests = TestList
  [ "strArrayToSExpr: Empty input" ~: assertEqual "Should return []" (strArrayToSExpr []) []
  , "strArrayToSExpr: Simple expressions" ~: assertEqual "Should return list of SExpr" (strArrayToSExpr ["1", "+", "2"]) [SInt 1, SSym "+", SInt 2]
  , "strArrayToSExpr: Nested expressions" ~: assertEqual "Should return nested SList" (strArrayToSExpr ["(", "1", "(", "2", "3", ")", "4", ")"]) [SList [SInt 1, SList [SInt 2, SInt 3], SInt 4]]
  ]

readSExprTests :: Test
readSExprTests = TestList
  [ "readSExpr: Integer" ~: assertEqual "Should return SInt" (readSExpr "42") (SInt 42)
  , "readSExpr: Float" ~: assertEqual "Should return SFloat" (readSExpr "3.14") (SFloat 3.14)
  , "readSExpr: Boolean" ~: assertEqual "Should return SBool" (readSExpr "#t") (SBool True)
  ]

sexprSplitTests :: Test
sexprSplitTests = TestList
  [ "sexprSplit: Empty input" ~: assertEqual "Should return []" (sexprSplit "") []
  , "sexprSplit: Simple input" ~: assertEqual "Should return split input" (sexprSplit "1 2 (3 4) 5") ["1", "2", "(", "3", "4", ")", "5"]
  ]

isBetweenTests :: Test
isBetweenTests = TestList
  [ "isBetween: Test1" ~: assertEqual "Should return True" (isBetween (5 :: Int) 1 10) True
  , "isBetween: Test2" ~: assertEqual "Should return False" (isBetween (15 :: Int) 1 10) False
  ]

getParenthesisTests :: Test
getParenthesisTests = TestList
  [ "getParenthesis: Empty array" ~: assertEqual "Should return []" (getParenthesis [] 0) []
  , "getParenthesis: Array with 1 closing parenthesis" ~: assertEqual "Should return [\")\"]" (getParenthesis [")"] 1) [")"]
  , "getParenthesis: Array with 2 opening and 2 closing parentheses" ~: assertEqual "Should return [\"(\", \")\"]" (getParenthesis ["(", ")", "(", ")"] 0) ["(", ")"]
  ]

removeParenthesisTests :: Test
removeParenthesisTests = TestList
  [ "removeParenthesis: Test1" ~: assertEqual "Should return without parentheses" (removeParenthesis ["(", "1", ")", "2", "3", "(", "4", ")", ")"] 0) ["2", "3", "(", "4", ")", ")"]
  , "removeParenthesis: Empty Array" ~: assertEqual "Should be return []" (removeParenthesis [] 0) []
  , "removeParenthesis: No Parenthesis" ~: assertEqual "Should be return the same array" (removeParenthesis ["1", "2", "3", "4"] 0) ["1", "2", "3", "4"]
  ]

getInsideParenthesesTests :: Test
getInsideParenthesesTests = TestList
  [ "getInsideParentheses: Test1" ~: assertEqual "Should return inside parentheses" (getInsideParentheses ["(", "1", "2", ")", "3", "(", "4", ")"]) ["1", "2"]
  , "getInsideParentheses: No parentheses" ~: assertEqual "Should return empty array" (getInsideParentheses ["1", "2", "3"]) []
  , "getInsideParentheses: Empty Array" ~: assertEqual "Should return empty array" (getInsideParentheses []) []
  ]
