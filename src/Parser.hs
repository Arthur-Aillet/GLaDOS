--
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- parser
--

module Parser  (removeLast, sExprParser, strArrayToSExpr, strToSExpr, sexprSplit, isBetween, strSplit, separateChar, separateCharOnList, cleanStrings, getParenthesis, removeParenthesis, removeFirstAndLast, getInsideParentheses) where

import Text.Read (readMaybe)

data SExpr  = SInt Int
            | SSym String
            | SList [SExpr]
            deriving (Show)

sExprParser :: String -> [SExpr]
sExprParser str = strArrayToSExpr (sexprSplit str)

strArrayToSExpr :: [String] -> [SExpr]
strArrayToSExpr [] = []
strArrayToSExpr ("(" : xs) = (SList (strArrayToSExpr (getInsideParentheses(("(" : xs)))) : strArrayToSExpr (removeParenthesis ("(" : xs) 0))
strArrayToSExpr (x : xs) = (strToSExpr x : strArrayToSExpr xs )

strToSExpr :: String -> SExpr
strToSExpr str = case readMaybe str :: Maybe Int of
                    Just i -> SInt i
                    _ -> SSym str

sexprSplit :: String -> [String]
sexprSplit input = cleanStrings (cleanStrings (separateCharOnList (separateCharOnList (strSplit input ' ') '(') ')') "") "\n"

isBetween :: Char -> Char -> Char -> Bool
isBetween c smaller bigger  | c > bigger = False
                            | c < smaller = False
                            | otherwise = True

strSplit :: String -> Char -> [String]
strSplit [] _ = [""]
strSplit (x : xs) c | x == c = ("" : strSplit xs c)
                    | otherwise = (x : head (strSplit xs c)) : tail (strSplit xs c)

separateChar :: String -> Char -> [String]
separateChar [] _ = [""]
separateChar (x: xs) c  | x == c = ("" : (c : "") : separateChar xs c)
                    | otherwise = (x: head (separateChar xs c)) : tail (separateChar xs c)

separateCharOnList :: [String] -> Char -> [String]
separateCharOnList [] _ = []
separateCharOnList (x : xs) c = separateChar x c ++ separateCharOnList xs c

cleanStrings :: [String] -> String -> [String]
cleanStrings [] _ = []
cleanStrings (x : xs) str   | x == str = cleanStrings xs str
                            | otherwise = (x : cleanStrings xs str)

getParenthesis :: [String] -> Int -> [String]
getParenthesis [] _ = []
getParenthesis (")" : _) 1 = [")"]
getParenthesis (x : xs) i   | x == "(" = (x : getParenthesis xs (i + 1))
                            | x == ")" = (x : getParenthesis xs (i - 1))
                            | otherwise = (x : getParenthesis xs i)

removeParenthesis :: [String] -> Int -> [String]
removeParenthesis [] _ = []
removeParenthesis (")" : xs) 1 = xs
removeParenthesis (x : xs) i   | x == "(" = (removeParenthesis xs (i + 1))
                            | x == ")" = (removeParenthesis xs (i - 1))
                            | otherwise = (removeParenthesis xs i)

removeLast :: [String] -> [String]
removeLast [] = []
removeLast [x] = [x]
removeLast (x : _ : []) = [x]
removeLast (x : xs) = x : removeLast xs

removeFirstAndLast :: [String] -> [String]
removeFirstAndLast [] = []
removeFirstAndLast [_] = []
removeFirstAndLast (_ : xs) = removeLast xs

getInsideParentheses :: [String] -> [String]
getInsideParentheses str = removeFirstAndLast (getParenthesis str 0)

