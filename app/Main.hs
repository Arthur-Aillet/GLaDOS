{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS Main file
-}

module Main (main) where

import AST (Context, emptyContext, evalAST)
import Converter (sexprToAST)
import SParser (SExpr, sExprParser)
import System.Exit
import System.IO (hGetContents', stdin)

testInput :: IO [SExpr]
testInput = do
  contents <- hGetContents' stdin
  return $ sExprParser contents

main :: IO ExitCode
main = do
  expr <- testInput
  loopOnCommands emptyContext expr

loopOnCommands :: Context -> [SExpr] -> IO ExitCode
loopOnCommands _ [] = exitSuccess
loopOnCommands ctx (expr : xs) = case sexprToAST expr of
  Just ast -> print res >> loopOnCommands newCtx xs
    where
      (newCtx, res) = evalAST ctx ast
  Nothing -> exitWith (ExitFailure 84)
