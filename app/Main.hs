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

-- wrap the scraper in a timeout loop to prevent apparent crash should
--  measures to avoid waiting on input to fail
main :: IO ExitCode
main = do
    status <- timeout (10 * 1000 * 1000) scraper
    case status of
        Just () -> exitSuccess
        Nothing -> putStrLn "#ERR: timedout" >> exitWith (ExitFailure 84)
