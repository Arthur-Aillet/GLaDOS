{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS Main file
-}

module Main (main) where

import AST (Context, displayAST, emptyContext, evalAST)
import Converter (sexprToAST)
import ParserError
import ParserSExpr
import ParserType (Parser (..))
import PositionType
import SyntaxParser (parseManyValidOrEmpty)
import System.Exit
import System.IO (BufferMode (..), hGetContents', hIsTerminalDevice, hSetBuffering, stdin, stdout)
import Data.List
import System.Console.Haskeline
    ( getInputLine,
      completeWord,
      simpleCompletion,
      runInputT,
      Completion,
      InputT,
      Settings(Settings, autoAddHistory, complete, historyFile), outputStrLn )
import System.Console.Haskeline.History
import Control.Monad.IO.Class

import System.Timeout (timeout)
import Data.HashMap.Internal.Strict (keys)

--would be sweet if i ever find how to expand this based on the context
keywords = ["(define", "(lambda", "(eq?", "(div", "(mod", "(if"]

search :: [String] -> String -> [Completion]
search symbols str = map simpleCompletion $ filter (str `isPrefixOf`) (keywords ++ symbols ++ map ('(' :) symbols)

executeFile :: IO ()
executeFile = do
  contents <- hGetContents' stdin
  case runParser (parseManyValidOrEmpty parseSExpr) contents defaultPosition of
    Left err -> printErr err >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> do
      _ <- loopOnCommands emptyContext sexpr
      exitSuccess

haskelineGetline :: InputT IO String
haskelineGetline = do
                    line <- getInputLine "\ESC[31m\STXG\ESC[0;33m\STXL\ESC[3;32m\STXa\ESC[0;32m\STXD\ESC[34m\STXO\ESC[35m\STXS\ESC[0m\STX>:"
                    case line of
                      Nothing -> return ""
                      Just str -> return str

getInstructions :: Context -> IO ()
getInstructions context = do
  print (keys context)
  new_line <- runInputT
                Settings {
                  complete = completeWord Nothing " \t" $ return . search (keys context),
                  historyFile = Just "history",
                  autoAddHistory = True
                }
                haskelineGetline
  case runParser (parseManyValidOrEmpty parseSExpr) new_line defaultPosition of
    Left err -> liftIO (printErr err) >> liftIO (exitWith (ExitFailure 84))
    Right (sexpr, _, _) -> do
      new_context <- liftIO (loopOnCommands context sexpr)
      getInstructions new_context

main :: IO ()
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
