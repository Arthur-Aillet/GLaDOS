module InputManagment (haskelineGetline)
where

import System.Console.Haskeline
import Control.Monad.IO.Class

import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)


keywords :: [String]
keywords = ["(define", "define", "(lambda", "lambda", "(eq?", "eq?", "(div", "div", "(mod", "mod", "(if", "if"]

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
                    input <- getInputLine "\ESC[38;5;45m\STXGL\ESC[0m\STXa\ESC[38;5;208m\STXDOS\ESC[0m\STX> "
                    case input of
                      Nothing -> return ""
                      Just str -> return str
