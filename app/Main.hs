{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
-}

module Main (main) where

import AST (Context, emptyContext, evalAST)
import System.Environment (getArgs, getProgName)
import Converter (sexprToAST)
import System.Timeout (timeout)
import SParser (SExpr, sExprParser)
import System.Exit
import System.IO (hGetContents', stdin, hIsTerminalDevice, hSetBuffering, stdout, BufferMode(..))

getInstructions :: Context -> IO ()
getInstructions context = do
                    putStr "glados >"
                    line <- getLine
                    _ <- loopOnCommands context (sExprParser line)
                    getInstructions context

-- wrap the scraper in a timeout loop to prevent apparent crash should
--  measures to avoid waiting on input to fail
main :: IO ExitCode
main = do
    hSetBuffering stdout NoBuffering
    getInstructions emptyContext
    exitSuccess

loopOnCommands :: Context -> [SExpr] -> IO ()
loopOnCommands _ [] = pure()
loopOnCommands ctx (expr : xs) = case sexprToAST expr of
  Just ast -> print res >> loopOnCommands newCtx xs
    where
      (newCtx, res) = evalAST ctx ast
  Nothing -> exitWith (ExitFailure 84)
