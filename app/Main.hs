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

-- print the command line, with exec name and all args seperated by a space
-- note the lack of quoting for params containing a space
cmd :: IO ()
cmd = do
    name <- getProgName
    args <- getArgs
    putStr name
    putStr " "
    mapM_ (\s -> putStr (' ':s)) args
    putStrLn ""

-- print the stdin or fail if stdin is a tty
cat :: IO ()
cat = do
    bool <- hIsTerminalDevice stdin
    if bool
        then
            putStrLn "#ERR: input is tty"
        else
            do
                contents <- hGetContents' stdin
                _ <- loopOnCommands emptyContext (sExprParser contents)
                pure()

-- dump input
scraper :: IO ()
scraper =  putStrLn "cmd:" >> cmd >> putStrLn "cat:" >> cat

getInstructions :: IO ()
getInstructions = do
                    putStr "glados >"
                    line <- getLine
                    _ <- loopOnCommands emptyContext (sExprParser line)
                    getInstructions

-- wrap the scraper in a timeout loop to prevent apparent crash should
--  measures to avoid waiting on input to fail
main :: IO ExitCode
main = do
    hSetBuffering stdout NoBuffering
    bool <- hIsTerminalDevice stdin
    if bool
        then do
            getInstructions
            exitSuccess
    else do
        status <- timeout (10 * 1000 * 1000) scrapers
        case status of
            Just () -> exitSuccess
            Nothing -> putStrLn "#ERR: timedout" >> exitWith (ExitFailure 84)

loopOnCommands :: Context -> [SExpr] -> IO ExitCode
loopOnCommands _ [] = exitSuccess
loopOnCommands ctx (expr : xs) = case sexprToAST expr of
  Just ast -> print res >> loopOnCommands newCtx xs
    where
      (newCtx, res) = evalAST ctx ast
  Nothing -> exitWith (ExitFailure 84)
