module ConverterSpec (converterTests) where

import Ast (Ast (AAtom, Call, Define, Func, If, Lambda, Symbol, Truth), Atom (AtomF, AtomI))
import Converter (sexprToAST)
import ParserSExpr (SExpr (SBool, SFloat, SInt, SList, SSym))
import Test.HUnit

converterTests :: Test
converterTests =
  TestList
    [ "sexprToAST" ~: sexprToASTTests
    ]

sexprToASTTests :: Test
sexprToASTTests =
  TestList
    [ "Test integer conversion" ~: sexprToAST (SInt 42) ~?= Just (AAtom (AtomI 42)),
      "Test float conversion" ~: sexprToAST (SFloat 3.14) ~?= Just (AAtom (AtomF 3.14)),
      "Test boolean conversion (true)" ~: sexprToAST (SBool True) ~?= Just (Truth True),
      "Test boolean conversion (false)" ~: sexprToAST (SBool False) ~?= Just (Truth False),
      "Test symbol conversion" ~: sexprToAST (SSym "foo") ~?= Just (Symbol "foo"),
      "Test single list conversion" ~: sexprToAST (SList [SInt 42]) ~?= Just (AAtom (AtomI 42)),
      "Test define with expression" ~: sexprToAST (SList [SSym "define", SSym "x", SInt 42]) ~?= Just (Define "x" (AAtom (AtomI 42))),
      "Test define with lambda" ~:
        sexprToAST (SList [SSym "define", SList [SSym "add", SSym "x", SSym "y"], SList [SSym "+", SSym "x", SSym "y"]])
          ~?= Just (Define "add" (Func "add" ["x", "y"] (Call (Symbol "+") [Symbol "x", Symbol "y"]))),
      "Test if expression" ~: sexprToAST (SList [SSym "if", SBool True, SInt 1, SInt 2]) ~?= Just (If (Truth True) (AAtom (AtomI 1)) (AAtom (AtomI 2))),
      "Test lambda expression" ~:
        sexprToAST (SList [SSym "lambda", SList [SSym "x", SSym "y"], SList [SSym "+", SSym "x", SSym "y"]])
          ~?= Just (Lambda ["x", "y"] (Call (Symbol "+") [Symbol "x", Symbol "y"])),
      "Test function call" ~: sexprToAST (SList [SSym "add", SInt 1, SInt 2]) ~?= Just (Call (Symbol "add") [AAtom (AtomI 1), AAtom (AtomI 2)]),
      "Test invalid input" ~: sexprToAST (SList []) ~?= Nothing
    ]
