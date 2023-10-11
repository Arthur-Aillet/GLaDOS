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
import System.Console.Haskeline
    ( getInputLine,
      completeWord,
      simpleCompletion,
      runInputT,
      Completion,
      InputT,
      Settings(Settings, autoAddHistory, complete, historyFile) )
import Control.Monad.IO.Class

import System.Timeout (timeout)
import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)

keywords :: [String]
keywords = ["(define", "define", "(lambda", "lambda", "(eq?",
            "eq?", "(div", "div", "(mod", "mod", "(if", "if"]

search :: [String] -> String -> [Completion]
search symbols str = map simpleCompletion $
    filter (str `isPrefixOf`) (keywords ++ symbols ++ map ('(' :) symbols)

executeFile :: IO ()
executeFile = do
  contents <- hGetContents' stdin
  case runParser (parseManyValidOrEmpty parseSExpr) contents defaultPosition of
    Left err -> printErr err >> exitWith (ExitFailure 84)
    Right (sexpr, _, _) -> do
      _ <- loopOnCommands emptyContext sexpr
      exitSuccess

inputKey :: String
inputKey = "\ESC[38;5;45m\STXGL\ESC[0m\STXa\ESC[38;5;208m\STXDOS\ESC[0m\STX> "

haskelineGetline :: InputT IO String
haskelineGetline = do
                    input <- getInputLine inputKey
                    case input of
                      Nothing -> return ""
                      Just str -> return str

newSettings ::  MonadIO m => Context -> Settings m
newSettings (context, _) = Settings {
                  complete = completeWord Nothing " \t" $
                    return . search (keys context),
                  historyFile = Just ".history",
                  autoAddHistory = True
                }

getInstructions :: Context -> IO ()
getInstructions (context, depth) = do
  new_line <- runInputT (newSettings (context, depth)) haskelineGetline
  case runParser (parseManyValidOrEmpty parseSExpr) new_line defaultPosition of
    Left err -> liftIO (printErr err) >> liftIO (exitWith (ExitFailure 84))

    Right (sexpr, _, _) -> do
      new_context <- liftIO (loopOnCommands (context, depth) sexpr)
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
  Just ast -> displayAST res >> loopOnCommands (newCtx, 0) xs
    where
      ((newCtx, _), res) = evalAST ctx ast
  Nothing -> return ctx
