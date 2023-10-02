{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS Main file
-}

module Main (main) where

import AST (Context, emptyContext, evalAST, displayAST)
import System.Environment (getArgs, getProgName)
import Converter (sexprToAST)
import System.Timeout (timeout)
import System.Exit
import System.IO (hGetContents', stdin, hIsTerminalDevice, hSetBuffering, stdout, BufferMode(..))

import ParserSExpr
import PositionType
import ParserType (Parser(..))
import SyntaxParser (parseManyValidOrEmpty)

executeFile :: IO ()
executeFile = do
  contents <- hGetContents' stdin
  case runParser (parseManyValidOrEmpty parseSExpr) contents defaultPosition of
    Left (err, pos) -> putStrLn (show err ++ " found at: " ++ show pos) >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> do
      _ <- loopOnCommands emptyContext sexpr
      exitSuccess

getInstructions :: Context -> IO ExitCode
getInstructions context = do
  putStr "GLaDOS > "
  line <- getLine
  case runParser (parseManyValidOrEmpty parseSExpr) line defaultPosition of
    Left (err, pos) -> putStrLn (show err ++ " found at: " ++ show pos) >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> do
      context <- loopOnCommands context sexpr
      getInstructions context

-- wrap the scraper in a timeout loop to prevent apparent crash should
--  measures to avoid waiting on input to fail
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
