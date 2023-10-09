{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS Main file
-}

module Main (main) where

import Ast (Context, displayAST, emptyContext, evalAST)
import Converter (sexprToAST)
import ParserError ( printErr )
import ParserSExpr ( parseSExpr, SExpr )
import ParserType (Parser (..))
import PositionType ( defaultPosition )
import SyntaxParser (parseManyValidOrEmpty)
import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith )
import System.IO (BufferMode (..), hGetContents', hIsTerminalDevice, hSetBuffering, stdin, stdout)
import System.Timeout (timeout)

executeFile :: IO ()
executeFile = do
  contents <- hGetContents' stdin
  case runParser (parseManyValidOrEmpty parseSExpr) contents defaultPosition of
    Left err -> printErr err >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> do
      _ <- loopOnCommands emptyContext sexpr
      exitSuccess

getInstructions :: Context -> IO ExitCode
getInstructions context = do
  putStr "GLaDOS> "
  new_line <- getLine
  case runParser (parseManyValidOrEmpty parseSExpr) new_line defaultPosition of
    Left err -> printErr err >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> do
      new_context <- loopOnCommands context sexpr
      getInstructions new_context

main :: IO ExitCode
main = do
  hSetBuffering stdout NoBuffering
  bool <- hIsTerminalDevice stdin
  if bool
    then getInstructions emptyContext >> exitSuccess
    else do
      status <- timeout 10000000 executeFile
      case status of
        Just () -> exitSuccess
        Nothing -> putStrLn "#ERR: timedout" >> exitWith (ExitFailure 84)

loopOnCommands :: Context -> [SExpr] -> IO Context
loopOnCommands ctx [] = return ctx
loopOnCommands ctx (expr : xs) = case sexprToAST expr of
  Just ast -> displayAST res >> loopOnCommands newCtx xs
    where
      (newCtx, res) = evalAST ctx ast
  Nothing -> return ctx
