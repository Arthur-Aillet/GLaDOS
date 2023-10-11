{-
-- EPITECH PROJECT, 2023
-- haskeline.hs
-- File description:
-- GLaDOS haskeline file
-}

module InputManagment (haskelineGetline) where

import Control.Monad.IO.Class
import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)
import System.Console.Haskeline

keywords2 :: [String]
keywords2 = ["define", "lambda", "eq?", "div", "mod", "if"]

keywords :: [String]
keywords = ["(define", "(lambda", "(eq?", "(div", "(mod", "(if"] ++ keywords2

search :: [String] -> String -> [Completion]
search symbols str =
  map simpleCompletion $
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
