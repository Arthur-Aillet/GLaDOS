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

import ParserSExpr
import PositionType
import ParserType (Parser(..))
import SyntaxParser (parseMany2)

main :: IO ExitCode
main = do
  contents <- hGetContents' stdin
  case runParser (parseMany2 parseSExpr) contents defaultPosition of
    Left (err, pos) -> putStrLn (show err ++ " found at: " ++ show pos) >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> print sexpr >> loopOnCommands emptyContext sexpr

loopOnCommands :: Context -> [SExpr] -> IO ExitCode
loopOnCommands _ [] = exitSuccess
loopOnCommands ctx (expr : xs) = case sexprToAST expr of
  Just ast -> print res >> loopOnCommands newCtx xs
    where
      (newCtx, res) = evalAST ctx ast
  Nothing -> exitWith (ExitFailure 84)
