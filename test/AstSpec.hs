module AstSpec (parserASTTests) where

import Ast
  ( Ast (AAtom, Builtin, Call, Define, Error, If, Lambda, Null, Symbol, Truth),
    Context,
    binOp,
    builtinDiv,
    builtinPredicates,
    builtinMod,
    emptyContext,
    evalAST,
    execBuiltins,
    execCall,
    execCallDistribute,
    expectAtom,
    isBuiltin,
    Predicates(..)
  )
import Data.HashMap.Lazy
import qualified Data.HashMap.Strict as HM
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
      "builtinPredicates" ~: builtinPredicatesTests,
      "builtinDiv" ~: builtinDivTests,
      "builtinMod" ~: builtinModTests
    ]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

exempleContext :: Int -> Context
exempleContext i = (HM.fromList [("Symbole", Symbol "define"), ("String", Symbol "var"), ("AAtom", AAtom 9)], i)

newContext :: Int -> Context
newContext i = (HM.fromList [("Symbole", Symbol "define"), ("var", AAtom 42), ("String", Symbol "var"), ("AAtom", AAtom 9)], i)

callDistributeResultContext :: Int -> Context
callDistributeResultContext i = (HM.fromList [("Symbole", Symbol "define"), ("b", AAtom 11), ("String", Symbol "var"), ("a", AAtom 10), ("AAtom", AAtom 9)], i)

addContext :: Int -> Context
addContext i = (HM.fromList [("add", Lambda ["a", "b"] (Builtin "+" [Symbol "a", Symbol "b"])), ("Symbole", Symbol "define"), ("String", Symbol "var"), ("AAtom", AAtom 9)], i)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

emptyContextTest :: Test
emptyContextTest = TestCase $ assertEqual "return empty" (empty, 0) emptyContext

execCallDistributeTests :: Test
execCallDistributeTests =
  TestList
    [ "end of list" ~: execCallDistribute (exempleContext 0) [] [] ~?= Just (exempleContext 0),
      "regular" ~: execCallDistribute (exempleContext 0) ["a", "b"] [AAtom 10, AAtom 11] ~?= Just (callDistributeResultContext 1),
      "nothing" ~: execCallDistribute (exempleContext 0) [] [AAtom 10] ~?= Nothing
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
    [ "builtin < false" ~: execBuiltins (exempleContext 0) "<" [AAtom 15, AAtom 10] ~?= Truth False,
      "builtin < true" ~: execBuiltins (exempleContext 0) "<" [AAtom 10, AAtom 15] ~?= Truth True,
      "builtin eq false" ~: execBuiltins (exempleContext 0) "eq?" [AAtom 15, AAtom 10] ~?= Truth False,
      "builtin eq true" ~: execBuiltins (exempleContext 0) "eq?" [AAtom 15, AAtom 15] ~?= Truth True,
      "builtin == false" ~: execBuiltins (exempleContext 0) "==" [AAtom 15, AAtom 10] ~?= Truth False,
      "builtin == true" ~: execBuiltins (exempleContext 0) "==" [AAtom 15, AAtom 15] ~?= Truth True,
      "builtin != false" ~: execBuiltins (exempleContext 0) "!=" [AAtom 15, AAtom 10] ~?= Truth True,
      "builtin != true" ~: execBuiltins (exempleContext 0) "!=" [AAtom 15, AAtom 15] ~?= Truth False,
      "builtin <= false" ~: execBuiltins (exempleContext 0) "<=" [AAtom 15, AAtom 10] ~?= Truth False,
      "builtin <= true and equal" ~: execBuiltins (exempleContext 0) "<=" [AAtom 15, AAtom 15] ~?= Truth True,
      "builtin <= true " ~: execBuiltins (exempleContext 0) "<=" [AAtom 15, AAtom 16] ~?= Truth True,
      "builtin >= true" ~: execBuiltins (exempleContext 0) ">=" [AAtom 15, AAtom 10] ~?= Truth True,
      "builtin >= true and equal" ~: execBuiltins (exempleContext 0) ">=" [AAtom 15, AAtom 15] ~?= Truth True,
      "builtin >= false" ~: execBuiltins (exempleContext 0) ">=" [AAtom 15, AAtom 16] ~?= Truth False,
      "builtin +" ~: execBuiltins (exempleContext 0) "+" [AAtom 15, AAtom 10] ~?= AAtom 25,
      "builtin -" ~: execBuiltins (exempleContext 0) "-" [AAtom 15, AAtom 10] ~?= AAtom 5,
      "builtin *" ~: execBuiltins (exempleContext 0) "*" [AAtom 15, AAtom 10] ~?= AAtom 150,
      "builtin div" ~: execBuiltins (exempleContext 0) "div" [AAtom 15, AAtom 10] ~?= AAtom 1,
      "builtin mod" ~: execBuiltins (exempleContext 0) "mod" [AAtom 15, AAtom 10] ~?= AAtom 5,
      "builtin unimplemented" ~: execBuiltins (exempleContext 0) "unimplemented" [AAtom 15, AAtom 10] ~?= Error "Symbol 'unimplemented' is not bound"
    ]

isBuiltinTests :: Test
isBuiltinTests =
  TestList
    [ "Test < is a builtin" ~: isBuiltin "<" ~?= True,
      "Test > is a builtin" ~: isBuiltin ">" ~?= True,
      "Test >= is a builtin" ~: isBuiltin ">=" ~?= True,
      "Test <= is a builtin" ~: isBuiltin "<=" ~?= True,
      "Test == is a builtin" ~: isBuiltin "==" ~?= True,
      "Test != is a builtin" ~: isBuiltin "!=" ~?= True,
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
    [ "Error" ~: ((exempleContext 1), Error "Tu es mauvais Jack") @=? (evalAST (exempleContext 0) (Error "Tu es mauvais Jack")),
      "Ast is Null" ~: ((exempleContext 1), Error "Expression has no value") @=? (evalAST (exempleContext 0) Null),
      "Find a value in context" ~: ((exempleContext 1), AAtom 9) @=? (evalAST (exempleContext 0) (Symbol "AAtom")),
      "Find operator" ~: ((exempleContext 1), Symbol "+") @=? (evalAST (exempleContext 0) (Symbol "+")),
      "Not find value in context" ~: ((exempleContext 1), Error "Symbol 'b' is not bound") @=? (evalAST (exempleContext 0) (Symbol "b")),
      "Define" ~: ((newContext 1), Null) @=? (evalAST (exempleContext 0) (Define "var" (AAtom 42))),
      "AAtom" ~: ((exempleContext 1), AAtom 42) @=? (evalAST (exempleContext 0) (AAtom 42)),
      "Truth" ~: ((exempleContext 1), Truth True) @=? (evalAST (exempleContext 0) (Truth True)),
      "Call" ~: ((addContext 0), AAtom 10) @=? evalAST (addContext 0) (Call (Symbol "add") [AAtom 4, AAtom 6]),
      "builtin" ~: ((exempleContext 1), Error "Symbol 'foo' is not bound") @=? evalAST (exempleContext 0) (Builtin "foo" []),
      "If Error" ~: ((exempleContext 1), Error "Expression has no value") @=? evalAST (exempleContext 0) (If Null (Truth True) (Truth False)),
      "If False" ~: ((exempleContext 2), Truth False) @=? evalAST (exempleContext 0) (If (Builtin "eq?" [AAtom 10, AAtom 5]) (Truth True) (Truth False)),
      "If True" ~: ((exempleContext 2), Truth True) @=? evalAST (exempleContext 0) (If (Builtin "eq?" [AAtom 10, AAtom 10]) (Truth True) (Truth False))
    ]

expectAtomTests :: Test
expectAtomTests =
  TestList
    [ "AAtom" ~: (AAtom 9) @=? (expectAtom ((exempleContext 0), AAtom 9)),
      "Truth" ~: (Truth True) @=? (expectAtom ((exempleContext 0), Truth True)),
      "String" ~: (Error ("Symbol 'b' is not bound")) @=? (expectAtom ((exempleContext 0), Symbol "b")),
      "Error" ~: (Error "Tu es mauvais Jack") @=? (expectAtom ((exempleContext 0), Error "Tu es mauvais Jack")),
      "Otherwise" ~: (Error ("Expected Atom but got: Define \"var\" (AAtom (AtomI 2))")) @=? (expectAtom ((exempleContext 0), Define "var" (AAtom 2)))
    ]

binOpTests :: Test
binOpTests =
  TestList
    [ "Addition" ~: binOp (+) (exempleContext 0) [AAtom 2, AAtom 3] ~?= AAtom 5,
      "Subtraction" ~: binOp (-) (exempleContext 0) [AAtom 5, AAtom 3] ~?= AAtom 2,
      "Multiplication" ~: binOp (*) (exempleContext 0) [AAtom 2, AAtom 3] ~?= AAtom 6,
      "Mutltiple arguments" ~: binOp (+) (exempleContext 0) [AAtom 2, AAtom 3, AAtom 4] ~?= AAtom 9,
      "Error first argument" ~: binOp (+) (exempleContext 0) [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: binOp (+) (exempleContext 0) [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound"
    ]

builtinPredicatesTests :: Test
builtinPredicatesTests =
  TestList
    [ "Equality True" ~: builtinPredicates Eq (exempleContext 0) [AAtom 3, AAtom 3] ~?= Truth True,
      "Equality False" ~: builtinPredicates Eq (exempleContext 0) [AAtom 3, AAtom 5] ~?= Truth False,
      "Equality error first argument" ~: builtinPredicates Eq (exempleContext 0) [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Equality error second argument" ~: builtinPredicates Eq (exempleContext 0) [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Equality error third argument" ~: builtinPredicates Eq (exempleContext 0) [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to predicate Eq",
      "Not Equality False" ~: builtinPredicates NEq (exempleContext 0) [AAtom 3, AAtom 3] ~?= Truth False,
      "Not Equality True" ~: builtinPredicates NEq (exempleContext 0) [AAtom 3, AAtom 5] ~?= Truth True,
      "Not Equality error first argument" ~: builtinPredicates NEq (exempleContext 0) [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Not Equality error second argument" ~: builtinPredicates NEq (exempleContext 0) [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Not Equality error third argument" ~: builtinPredicates NEq (exempleContext 0) [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to predicate NEq",
      "Lower than False" ~: builtinPredicates Lt (exempleContext 0) [AAtom 3, AAtom 3] ~?= Truth False,
      "Lower than True" ~: builtinPredicates Lt (exempleContext 0) [AAtom 3, AAtom 5] ~?= Truth True,
      "Lower than not equal False" ~: builtinPredicates Lt (exempleContext 0) [AAtom 3, AAtom 3] ~?= Truth False,
      "Lower than error first argument" ~: builtinPredicates Lt (exempleContext 0) [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Lower than error second argument" ~: builtinPredicates Lt (exempleContext 0) [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Lower than error third argument" ~: builtinPredicates Lt (exempleContext 0) [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to predicate Lt"
    ]

builtinDivTests :: Test
builtinDivTests =
  TestList
    [ "Division" ~: builtinDiv (exempleContext 0) [AAtom 6, AAtom 3] ~?= AAtom 2,
      "Division by Zero" ~: builtinDiv (exempleContext 0) [AAtom 6, AAtom 0] ~?= Error "Division by zero is denied",
      "Multiple arguments" ~: builtinDiv (exempleContext 0) [AAtom 10, AAtom 2, AAtom 2] ~?= AAtom 2,
      "Error first argument" ~: builtinDiv (exempleContext 0) [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinDiv (exempleContext 0) [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound"
    ]

builtinModTests :: Test
builtinModTests =
  TestList
    [ "Modulo" ~: builtinMod (exempleContext 0) [AAtom 7, AAtom 3] ~?= AAtom 1,
      "Modulo by Zero" ~: builtinMod (exempleContext 0) [AAtom 7, AAtom 0] ~?= Error "Modulo by zero is denied",
      "Error first argument" ~: builtinMod (exempleContext 0) [Symbol "foo", AAtom 3] ~?= Error "Symbol 'foo' is not bound",
      "Error second argument" ~: builtinMod (exempleContext 0) [AAtom 3, Symbol "foo"] ~?= Error "Symbol 'foo' is not bound",
      "Error third argument" ~: builtinMod (exempleContext 0) [AAtom 2, AAtom 3, AAtom 4] ~?= Error "Bad number of args to mod"
    ]
