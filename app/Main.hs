{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
-}

module Main (main) where

import System.Environment (getArgs, getProgName)
import System.IO (stdin, hGetContents', hIsTerminalDevice)
import System.Timeout (timeout)
import System.Exit
import SParser (sExprParser, SExpr)
import Converter (sexprToAST)
import AST (evalAST, Context, emptyContext)

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
                putStrLn contents

-- dump input
scraper :: IO ()
scraper =  putStrLn "cmd:" >> cmd >> putStrLn "cat:" >> cat

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
loopOnCommands ctx (expr:xs) = case sexprToAST expr of
    Just ast -> print res >> loopOnCommands newCtx xs
        where (newCtx, res) = evalAST ctx ast
    Nothing -> exitWith (ExitFailure 84)


-- wrap the scraper in a timeout loop to prevent apparent crash should
--  measures to avoid waiting on input to fail
main2 :: IO ExitCode
main2 = do
    status <- timeout (10 * 1000 * 1000) scraper
    case status of
        Just () -> exitSuccess
        Nothing -> putStrLn "#ERR: timedout" >> exitWith (ExitFailure 84)
