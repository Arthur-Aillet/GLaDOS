module AstSpec (parserASTTests) where

import Data.HashMap.Lazy
import Test.HUnit
import Ast (Ast (Symbol, Define, Atom, Truth, Lambda, Func, Call, Builtin, If, Error),
    evalAST,
    displayAST,
    Context,
    emptyContext,
    isBuiltin,
    expectAtom
  )


parserASTTests :: Test
parserASTTests =
  TestList
    [ "emptyContext" ~: emptyContextTest,
      "isBuiltin" ~: isBuiltinTests,
      "expectAtom" ~: expectAtomTests
    ]

exempleContext = fromList [("Symbole" :: String, Symbol "define"), ("String", Symbol "var"), ("Atom", Atom 9)]

emptyContextTest :: Test
emptyContextTest = TestCase $ assertEqual "return empty" empty emptyContext

isBuiltinTests :: Test
isBuiltinTests = TestList
  [ "Test < is a builtin" ~: isBuiltin "<" ~?= True
    , "Test eq? is a builtin" ~: isBuiltin "eq?" ~?= True
    , "Test + is a builtin" ~: isBuiltin "+" ~?= True
    , "Test - is a builtin" ~: isBuiltin "-" ~?= True
    , "Test * is a builtin" ~: isBuiltin "*" ~?= True
    , "Test div is a builtin" ~: isBuiltin "div" ~?= True
    , "Test mod is a builtin" ~: isBuiltin "mod" ~?= True
    , "Test foo is not a builtin" ~: isBuiltin "foo" ~?= False
    ]

expectAtomTests :: Test
expectAtomTests = TestList
  [ "Atom" ~: (Atom 9) @=? (expectAtom (exempleContext, Atom 9))
  , "Truth" ~: (Truth True) @=? (expectAtom (exempleContext, Truth True))
  , "String" ~: (Error ("Symbol 'b' is not bound")) @=? (expectAtom (exempleContext, Symbol "b"))
  , "Error" ~: (Error "Tu es mauvais Jack") @=? (expectAtom (exempleContext, Error "Tu es mauvais Jack"))
  , "Other" ~: (Error ("expected Atom but got: Define \"var\" (Atom 2)")) @=? (expectAtom (exempleContext, Define "var" (Atom 2)))
  ]


