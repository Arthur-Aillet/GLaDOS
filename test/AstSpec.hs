module AstSpec (parserASTTests) where

import Ast
  ( Ast (Atom, Builtin, Call, Define, Error, Func, If, Lambda, Null, Symbol, Truth),
    Context,
    binOp,
    builtinDiv,
    builtinEq,
    builtinLt,
    builtinMod,
    displayAST,
    emptyContext,
    evalAST,
    execBuiltins,
    execCall,
    execCallDistribute,
    expectAtom,
    isBuiltin,
  )
import Data.Bool (Bool (True))
import Data.HashMap.Lazy
import GHC.Plugins (lambda)
import Test.HUnit

parserASTTests :: Test
parserASTTests =
  TestList
    [ "emptyContext" ~: emptyContextTest,
      "execCallDistribute" ~: execCallDistributeTests,
      "execCall" ~: execCallTests,
      "execBuiltins" ~: execBuiltinsTests,
      "isBuiltin" ~: isBuiltinTests,
      "expectAtom" ~: expectAtomTests,
      "evalAST" ~: evalASTTests,
      "binOp" ~: binOpTests,
      "builtinEq" ~: builtinEqTests,
      "builtinLt" ~: builtinLtTests,
      "builtinDiv" ~: builtinDivTests,
      "builtinMod" ~: builtinModTests
    ]

exempleContext = fromList [("Symbole" :: String, Symbol "define"), ("String", Symbol "var"), ("Atom", Atom 9)]

newContext = fromList [("Symbole", Symbol "define"), ("var", Atom 42), ("String", Symbol "var"), ("Atom", Atom 9)]

callDistributeResultContext = fromList [("Symbole", Symbol "define"), ("b", Atom 11), ("String", Symbol "var"), ("a", Atom 10), ("Atom", Atom 9)]

addContext = fromList [("add", Lambda ["a", "b"] (Builtin "+" [Symbol "a", Symbol "b"])), ("Symbole", Symbol "define"), ("String", Symbol "var"), ("Atom", Atom 9)]

emptyContextTest :: Test
emptyContextTest = TestCase $ assertEqual "return empty" empty emptyContext

execCallDistributeTests :: Test
execCallDistributeTests =
  TestList
    [ "end of list" ~: execCallDistribute exempleContext [] [] ~?= Just exempleContext,
      "regular" ~: execCallDistribute exempleContext ["a", "b"] [Atom 10, Atom 11] ~?= Just callDistributeResultContext,
      "nothing" ~: execCallDistribute exempleContext [] [Atom 10] ~?= Nothing
    ]

execCallTests :: Test
execCallTests =
  TestList
    [ "Call to Lambda with Correct Arguments" ~: execCall emptyContext (Lambda ["x", "y"] (Builtin "+" [Symbol "x", Symbol "y"])) [Atom 42, Atom 10] ~?= (emptyContext, Atom 52),
      "Call to Lambda with Incorrect Arguments" ~: execCall emptyContext (Lambda ["x", "y"] (Builtin "+" [Symbol "x", Symbol "y"])) [Atom 42] ~?= (emptyContext, Error "incorrect args to lambda"),
      "Call to Builtin Addition" ~: execCall emptyContext (Symbol "+") [Atom 5, Atom 7] ~?= (emptyContext, Atom 12),
      "Call to Non-Procedure Symbol" ~: execCall emptyContext (Symbol "foo") [] ~?= (emptyContext, Error "Symbol 'foo' is not bound"),
      "Ast is Null" ~: execCall emptyContext Null [Atom 42] ~?= (emptyContext, Error "expression has no value")
    ]

execBuiltinsTests :: Test
execBuiltinsTests =
  TestList
    [ "builtin < false" ~: execBuiltins exempleContext "<" [Atom 15, Atom 10] ~?= Truth False,
      "builtin < true" ~: execBuiltins exempleContext "<" [Atom 10, Atom 15] ~?= Truth True,
      "builtin eq false" ~: execBuiltins exempleContext "eq?" [Atom 15, Atom 10] ~?= Truth False,
      "builtin eq true" ~: execBuiltins exempleContext "eq?" [Atom 15, Atom 15] ~?= Truth True,
      "builtin +" ~: execBuiltins exempleContext "+" [Atom 15, Atom 10] ~?= Atom 25,
      "builtin -" ~: execBuiltins exempleContext "-" [Atom 15, Atom 10] ~?= Atom 5,
      "builtin *" ~: execBuiltins exempleContext "*" [Atom 15, Atom 10] ~?= Atom 150,
      "builtin div" ~: execBuiltins exempleContext "div" [Atom 15, Atom 10] ~?= Atom 1,
      "builtin mod" ~: execBuiltins exempleContext "mod" [Atom 15, Atom 10] ~?= Atom 5,
      "builtin unimplemented" ~: execBuiltins exempleContext "unimplemented" [Atom 15, Atom 10] ~?= Error "unimplemented builtin: unimplemented"
    ]

isBuiltinTests :: Test
isBuiltinTests =
  TestList
    [ "Test < is a builtin" ~: isBuiltin "<" ~?= True,
      "Test eq? is a builtin" ~: isBuiltin "eq?" ~?= True,
      "Test + is a builtin" ~: isBuiltin "+" ~?= True,
      "Test - is a builtin" ~: isBuiltin "-" ~?= True,
      "Test * is a builtin" ~: isBuiltin "*" ~?= True,
      "Test div is a builtin" ~: isBuiltin "div" ~?= True,
      "Test mod is a builtin" ~: isBuiltin "mod" ~?= True,
      "Test foo is not a builtin" ~: isBuiltin "foo" ~?= False
    ]

evalASTTests :: Test
evalASTTests =
  TestList
    [ "Error" ~: (exempleContext, Error "Tu es mauvais Jack") @=? (evalAST exempleContext (Error "Tu es mauvais Jack")),
      "Ast is Null" ~: (exempleContext, Error "expression has no value") @=? (evalAST exempleContext Null),
      "Find a value in context" ~: (exempleContext, Atom 9) @=? (evalAST exempleContext (Symbol "Atom")),
      "Find operator" ~: (exempleContext, Symbol "+") @=? (evalAST exempleContext (Symbol "+")),
      "Not find value in context" ~: (exempleContext, Error "Symbol 'b' is not bound") @=? (evalAST exempleContext (Symbol "b")),
      "Define" ~: (newContext, Null) @=? (evalAST exempleContext (Define "var" (Atom 42))),
      "Atom" ~: (exempleContext, Atom 42) @=? (evalAST exempleContext (Atom 42)),
      "Truth" ~: (exempleContext, Truth True) @=? (evalAST exempleContext (Truth True)),
      "Call" ~: (addContext, Atom 10) @=? evalAST addContext (Call (Symbol "add") [Atom 4, Atom 6]),
      "builtin" ~: (exempleContext, Error "unimplemented builtin: foo") @=? evalAST exempleContext (Builtin "foo" []),
      "If Error" ~: (exempleContext, Error "expression has no value") @=? evalAST exempleContext (If Null (Truth True) (Truth False)),
      "If False" ~: (exempleContext, Truth False) @=? evalAST exempleContext (If (Builtin "eq?" [Atom 10, Atom 5]) (Truth True) (Truth False)),
      "If True" ~: (exempleContext, Truth True) @=? evalAST exempleContext (If (Builtin "eq?" [Atom 10, Atom 10]) (Truth True) (Truth False))
    ]

expectAtomTests :: Test
expectAtomTests =
  TestList
    [ "Atom" ~: (Atom 9) @=? (expectAtom (exempleContext, Atom 9)),
      "Truth" ~: (Truth True) @=? (expectAtom (exempleContext, Truth True)),
      "String" ~: (Error ("Symbol 'b' is not bound")) @=? (expectAtom (exempleContext, Symbol "b")),
      "Error" ~: (Error "Tu es mauvais Jack") @=? (expectAtom (exempleContext, Error "Tu es mauvais Jack")),
      "Otherwise" ~: (Error ("expected Atom but got: Define \"var\" (Atom 2)")) @=? (expectAtom (exempleContext, Define "var" (Atom 2)))
    ]

binOpTests :: Test
binOpTests =
  TestList
    [ "Addition" ~: binOp (+) exempleContext [Atom 2, Atom 3] ~?= Atom 5,
      "Subtraction" ~: binOp (-) exempleContext [Atom 5, Atom 3] ~?= Atom 2,
      "Multiplication" ~: binOp (*) exempleContext [Atom 2, Atom 3] ~?= Atom 6,
      "Error first argument" ~: binOp (+) exempleContext [Symbol "foo", Atom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: binOp (+) exempleContext [Atom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: binOp (+) exempleContext [Atom 2, Atom 3, Atom 4] ~?= Error "Bad number of args to binary operand"
    ]

builtinEqTests :: Test
builtinEqTests =
  TestList
    [ "Equality True" ~: builtinEq exempleContext [Atom 3, Atom 3] ~?= Truth True,
      "Equality False" ~: builtinEq exempleContext [Atom 3, Atom 5] ~?= Truth False,
      "Error first argument" ~: builtinEq exempleContext [Symbol "foo", Atom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinEq exempleContext [Atom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinEq exempleContext [Atom 2, Atom 3, Atom 4] ~?= Error "Bad number of args to eq?"
    ]

builtinLtTests :: Test
builtinLtTests =
  TestList
    [ "Less Than True" ~: builtinLt exempleContext [Atom 2, Atom 3] ~?= Truth True,
      "Less Than False" ~: builtinLt exempleContext [Atom 3, Atom 2] ~?= Truth False,
      "Error first argument" ~: builtinLt exempleContext [Symbol "foo", Atom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinLt exempleContext [Atom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinLt exempleContext [Atom 2, Atom 3, Atom 4] ~?= Error "Bad number of args to <"
    ]

builtinDivTests :: Test
builtinDivTests =
  TestList
    [ "Division" ~: builtinDiv exempleContext [Atom 6, Atom 3] ~?= Atom 2,
      "Division by Zero" ~: builtinDiv exempleContext [Atom 6, Atom 0] ~?= Error "division by zero",
      "Error first argument" ~: builtinDiv exempleContext [Symbol "foo", Atom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinDiv exempleContext [Atom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinDiv exempleContext [Atom 2, Atom 3, Atom 4] ~?= Error "Bad number of args to div"
    ]

builtinModTests :: Test
builtinModTests =
  TestList
    [ "Modulo" ~: builtinMod exempleContext [Atom 7, Atom 3] ~?= Atom 1,
      "Modulo by Zero" ~: builtinMod exempleContext [Atom 7, Atom 0] ~?= Error "modulo by zero",
      "Error first argument" ~: builtinMod exempleContext [Symbol "foo", Atom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinMod exempleContext [Atom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinMod exempleContext [Atom 2, Atom 3, Atom 4] ~?= Error "Bad number of args to mod"
    ]
