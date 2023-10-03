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

-- isBuiltinTests :: Test
-- isBuiltinTests = TestList
--   [ "Test < " ~: True @=? (isBuiltin "<")
--     "Test eq? " ~: True @=? (isBuiltin "eq?"),
--     "Test + " ~: True @=? (isBuiltin "+"),
--     "Test - " ~: True @=? (isBuiltin "-"),
--     "Test * " ~: True @=? (isBuiltin "*"),
--     "Test div " ~: True @=? (isBuiltin "div"),
--     "Test mod " ~: True @=? (isBuiltin "mod"),
--     "Test Error " ~: False @=? (isBuiltin "foo")
--   ]

-- expectAtomTests :: Test
-- expectAtomTests = TestList
--   [ "Atom" ~: (Atom 9) @=? (expectAtom (exempleContext, Atom 9))
--   ]
