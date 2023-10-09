module AstSpec (parserASTTests) where

import Ast
  ( Ast (AAtom, Builtin, Call, Define, Error, Func, If, Lambda, Null, Symbol, Truth),
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

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

exempleContext :: Context
exempleContext = fromList [("Symbole" :: String, Symbol "define"), ("String", Symbol "var"), ("AAtom", AAtom 9)]

newContext :: Context
newContext = fromList [("Symbole", Symbol "define"), ("var", AAtom 42), ("String", Symbol "var"), ("AAtom", AAtom 9)]

callDistributeResultContext :: Context
callDistributeResultContext = fromList [("Symbole", Symbol "define"), ("b", AAtom 11), ("String", Symbol "var"), ("a", AAtom 10), ("AAtom", AAtom 9)]

addContext :: Context
addContext = fromList [("add", Lambda ["a", "b"] (Builtin "+" [Symbol "a", Symbol "b"])), ("Symbole", Symbol "define"), ("String", Symbol "var"), ("AAtom", AAtom 9)]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

emptyContextTest :: Test
emptyContextTest = TestCase $ assertEqual "return empty" empty emptyContext

execCallDistributeTests :: Test
execCallDistributeTests =
  TestList
    [ "end of list" ~: execCallDistribute exempleContext [] [] ~?= Just exempleContext,
      "regular" ~: execCallDistribute exempleContext ["a", "b"] [AAtom 10, AAtom 11] ~?= Just callDistributeResultContext,
      "nothing" ~: execCallDistribute exempleContext [] [AAtom 10] ~?= Nothing
    ]

execCallTests :: Test
execCallTests =
  TestList
    [ "Call to Lambda with Correct Arguments" ~: execCall emptyContext (Lambda ["x", "y"] (Builtin "+" [Symbol "x", Symbol "y"])) [AAtom 42, AAtom 10] ~?= (emptyContext, AAtom 52),
      "Call to Lambda with Incorrect Arguments" ~: execCall emptyContext (Lambda ["x", "y"] (Builtin "+" [Symbol "x", Symbol "y"])) [AAtom 42] ~?= (emptyContext, Error "Incorrect args to lambda"),
      "Call to Builtin Addition" ~: execCall emptyContext (Symbol "+") [AAtom 5, AAtom 7] ~?= (emptyContext, AAtom 12),
      "Call to Non-Procedure Symbol" ~: execCall emptyContext (Symbol "foo") [] ~?= (emptyContext, Error "Symbol 'foo' is not bound"),
      "Ast is Null" ~: execCall emptyContext Null [AAtom 42] ~?= (emptyContext, Error "Expression has no value")
    ]

execBuiltinsTests :: Test
execBuiltinsTests =
  TestList
    [ "builtin < false" ~: execBuiltins exempleContext "<" [AAtom 15, AAtom 10] ~?= Truth False,
      "builtin < true" ~: execBuiltins exempleContext "<" [AAtom 10, AAtom 15] ~?= Truth True,
      "builtin eq false" ~: execBuiltins exempleContext "eq?" [AAtom 15, AAtom 10] ~?= Truth False,
      "builtin eq true" ~: execBuiltins exempleContext "eq?" [AAtom 15, AAtom 15] ~?= Truth True,
      "builtin +" ~: execBuiltins exempleContext "+" [AAtom 15, AAtom 10] ~?= AAtom 25,
      "builtin -" ~: execBuiltins exempleContext "-" [AAtom 15, AAtom 10] ~?= AAtom 5,
      "builtin *" ~: execBuiltins exempleContext "*" [AAtom 15, AAtom 10] ~?= AAtom 150,
      "builtin div" ~: execBuiltins exempleContext "div" [AAtom 15, AAtom 10] ~?= AAtom 1,
      "builtin mod" ~: execBuiltins exempleContext "mod" [AAtom 15, AAtom 10] ~?= AAtom 5,
      "builtin unimplemented" ~: execBuiltins exempleContext "unimplemented" [AAtom 15, AAtom 10] ~?= Error "Symbol 'unimplemented' is not bound"
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
      "Ast is Null" ~: (exempleContext, Error "Expression has no value") @=? (evalAST exempleContext Null),
      "Find a value in context" ~: (exempleContext, AAtom 9) @=? (evalAST exempleContext (Symbol "AAtom")),
      "Find operator" ~: (exempleContext, Symbol "+") @=? (evalAST exempleContext (Symbol "+")),
      "Not find value in context" ~: (exempleContext, Error "Symbol 'b' is not bound") @=? (evalAST exempleContext (Symbol "b")),
      "Define" ~: (newContext, Null) @=? (evalAST exempleContext (Define "var" (AAtom 42))),
      "AAtom" ~: (exempleContext, AAtom 42) @=? (evalAST exempleContext (AAtom 42)),
      "Truth" ~: (exempleContext, Truth True) @=? (evalAST exempleContext (Truth True)),
      "Call" ~: (addContext, AAtom 10) @=? evalAST addContext (Call (Symbol "add") [AAtom 4, AAtom 6]),
      "builtin" ~: (exempleContext, Error "Symbol 'foo' is not bound") @=? evalAST exempleContext (Builtin "foo" []),
      "If Error" ~: (exempleContext, Error "Expression has no value") @=? evalAST exempleContext (If Null (Truth True) (Truth False)),
      "If False" ~: (exempleContext, Truth False) @=? evalAST exempleContext (If (Builtin "eq?" [AAtom 10, AAtom 5]) (Truth True) (Truth False)),
      "If True" ~: (exempleContext, Truth True) @=? evalAST exempleContext (If (Builtin "eq?" [AAtom 10, AAtom 10]) (Truth True) (Truth False))
    ]

expectAtomTests :: Test
expectAtomTests =
  TestList
    [ "AAtom" ~: (AAtom 9) @=? (expectAtom (exempleContext, AAtom 9)),
      "Truth" ~: (Truth True) @=? (expectAtom (exempleContext, Truth True)),
      "String" ~: (Error ("Symbol 'b' is not bound")) @=? (expectAtom (exempleContext, Symbol "b")),
      "Error" ~: (Error "Tu es mauvais Jack") @=? (expectAtom (exempleContext, Error "Tu es mauvais Jack")),
      "Otherwise" ~: (Error ("Expected Atom but got: Define \"var\" (AAtom (AtomI 2))")) @=? (expectAtom (exempleContext, Define "var" (AAtom 2)))
    ]

binOpTests :: Test
binOpTests =
  TestList
    [ "Addition" ~: binOp (+) exempleContext [AAtom 2, AAtom 3] ~?= AAtom 5,
      "Subtraction" ~: binOp (-) exempleContext [AAtom 5, AAtom 3] ~?= AAtom 2,
      "Multiplication" ~: binOp (*) exempleContext [AAtom 2, AAtom 3] ~?= AAtom 6,
      "Mutltiple arguments" ~: binOp (+) exempleContext [AAtom 2, AAtom 3, AAtom 4] ~?= AAtom 9,
      "Error first argument" ~: binOp (+) exempleContext [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: binOp (+) exempleContext [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound"
    ]

builtinEqTests :: Test
builtinEqTests =
  TestList
    [ "Equality True" ~: builtinEq exempleContext [AAtom 3, AAtom 3] ~?= Truth True,
      "Equality False" ~: builtinEq exempleContext [AAtom 3, AAtom 5] ~?= Truth False,
      "Error first argument" ~: builtinEq exempleContext [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinEq exempleContext [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinEq exempleContext [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to eq?"
    ]

builtinLtTests :: Test
builtinLtTests =
  TestList
    [ "Less Than True" ~: builtinLt exempleContext [AAtom 2, AAtom 3] ~?= Truth True,
      "Less Than False" ~: builtinLt exempleContext [AAtom 3, AAtom 2] ~?= Truth False,
      "Error first argument" ~: builtinLt exempleContext [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinLt exempleContext [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinLt exempleContext [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to <"
    ]

builtinDivTests :: Test
builtinDivTests =
  TestList
    [ "Division" ~: builtinDiv exempleContext [AAtom 6, AAtom 3] ~?= AAtom 2,
      "Division by Zero" ~: builtinDiv exempleContext [AAtom 6, AAtom 0] ~?= Error "Division by zero is denied",
      "Multiple arguments" ~: builtinDiv exempleContext [AAtom 10, AAtom 2, AAtom 2] ~?= AAtom 2,
      "Error first argument" ~: builtinDiv exempleContext [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinDiv exempleContext [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound"
    ]

builtinModTests :: Test
builtinModTests =
  TestList
    [ "Modulo" ~: builtinMod exempleContext [AAtom 7, AAtom 3] ~?= AAtom 1,
      "Modulo by Zero" ~: builtinMod exempleContext [AAtom 7, AAtom 0] ~?= Error "Modulo by zero is denied",
      "Error first argument" ~: builtinMod exempleContext [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinMod exempleContext [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinMod exempleContext [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to mod"
    ]
