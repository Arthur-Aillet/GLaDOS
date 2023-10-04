module AstSpec (parserASTTests) where

import Data.HashMap.Lazy
import Test.HUnit
import Ast (Ast (Symbol, Define, Atom, Truth, Lambda, Func, Call, Builtin, If, Error, Null),
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
      -- "execCallDistribute" ~: execCallDistributeTests,
      -- "execCall" ~: execCallTests,
      -- "execBuiltins" ~: execBuiltinsTests,
      "isBuiltin" ~: isBuiltinTests,
      "expectAtom" ~: expectAtomTests,
      "evalAST" ~: evalASTTests,
      -- "binOp" ~: binOpTests,
      -- "builtinEq" ~: builtinEqTests,
      -- "builtinLt" ~: builtinLtTests,
      -- "builtinDiv" ~: builtinDivTests,
      -- "builtinMod" ~: builtinModTests,
    ]

exempleContext = fromList [("Symbole" :: String, Symbol "define"), ("String", Symbol "var"), ("Atom", Atom 9)]
newContext = fromList [("Symbole",Symbol "define"),("var",Atom 42),("String",Symbol "var"),("Atom",Atom 9)]

emptyContextTest :: Test
emptyContextTest = TestCase $ assertEqual "return empty" empty emptyContext

-- execCallDistributeTests :: Test
-- execCallDistributeTests = TestList
--   [
--   ]

-- execCallTests :: Test
-- execCallTests = TestList
--   [
--   ]

-- execBuiltinsTests :: Test
-- execBuiltinsTests = TestList
--   [
--   ]

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


evalASTTests :: Test
evalASTTests = TestList
  [ "Error" ~: (exempleContext, Error "Tu es mauvais Jack") @=? (evalAST exempleContext (Error "Tu es mauvais Jack"))
  , "Ast is Null" ~: (exempleContext, Error "expression has no value") @=? (evalAST exempleContext Null)
  , "Find a value in context" ~: (exempleContext, Atom 9) @=? (evalAST exempleContext (Symbol "Atom"))
  , "Find operator" ~: (exempleContext, Symbol "+") @=? (evalAST exempleContext (Symbol "+"))
  , "Not find value in context" ~: (exempleContext, Error "Symbol 'b' is not bound") @=? (evalAST exempleContext (Symbol "b"))
  , "Define" ~: (newContext, Null) @=? (evalAST exempleContext (Define "var" (Atom 42)))
  , "Atom" ~: (exempleContext, Atom 42) @=? (evalAST exempleContext (Atom 42))
  , "Truth" ~: (exempleContext, Truth True) @=? (evalAST exempleContext (Truth True))
  -- , "Call" ~:
  -- , "Builtin" ~:
  --, "If Error" ~:
  --, "If False" ~:
  --, "If True" ~:
  --, "Otherwise" ~:
  ]

expectAtomTests :: Test
expectAtomTests = TestList
  [ "Atom" ~: (Atom 9) @=? (expectAtom (exempleContext, Atom 9))
  , "Truth" ~: (Truth True) @=? (expectAtom (exempleContext, Truth True))
  , "String" ~: (Error ("Symbol 'b' is not bound")) @=? (expectAtom (exempleContext, Symbol "b"))
  , "Error" ~: (Error "Tu es mauvais Jack") @=? (expectAtom (exempleContext, Error "Tu es mauvais Jack"))
  , "Otherwise" ~: (Error ("expected Atom but got: Define \"var\" (Atom 2)")) @=? (expectAtom (exempleContext, Define "var" (Atom 2)))
  ]

-- binOpTests :: Test
-- binOpTests = TestList
--   [
--   ]

-- builtinEqTests :: Test
-- builtinEqTests = TestList
--   [
--   ]

-- builtinLtTests :: Test
-- builtinLtTests = TestList
--   [
--   ]

-- builtinDivTests :: Test
-- builtinDivTests = TestList
--   [
--   ]

-- builtinModTests :: Test
-- builtinModTests = TestList
--   [
--   ]
