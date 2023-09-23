--
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- parser
--

module SParser
        (SExpr (SInt, SBool, SFloat, SSym, SList)
        ,sExprParser
        , strArrayToSExpr
        , readSExpr
        , sexprSplit
        , isBetween
        , removeParenthesis
        , getParenthesis
        , getInsideParentheses
        , readBool
        , separateChar
        , separateCharOnList
        , clean
        ) where

import Text.Read (readMaybe)
import GHC.Utils.Misc (split)

data SExpr  = SInt Int
            | SBool Bool
            | SFloat Float
            | SSym String
            | SList [SExpr]
            deriving (Show, Eq)

sExprParser :: String -> [SExpr]
sExprParser str = strArrayToSExpr (sexprSplit str)

strArrayToSExpr :: [String] -> [SExpr]
strArrayToSExpr [] = []
strArrayToSExpr ("(" : xs) = (SList (strArrayToSExpr inParenthesis) : strArrayToSExpr withoutParenthesis)
    where   inParenthesis = (getInsideParentheses(("(" : xs)))
            withoutParenthesis = (removeParenthesis ("(" : xs) 0)
strArrayToSExpr (x : xs) = (readSExpr x : strArrayToSExpr xs )


readBool :: String -> Maybe Bool
readBool "#f" = Just False
readBool "#t" = Just True
readBool _ = Nothing

readSExpr :: String -> SExpr
readSExpr str = case (readMaybe str :: Maybe Int, readMaybe str :: Maybe Float, readBool str) of
                    (_, _, Just b) -> SBool b
                    (Just i, _, _) -> SInt i
                    (_, Just f, _) -> SFloat f
                    _ -> SSym str

sexprSplit :: String -> [String]
sexprSplit input = cleanedUp
    where   cleanedUp = clean (clean (splitIntput) "") "\n"
            splitIntput = separateCharOnList (separateCharOnList spaceSplitInput '(') ')'
            spaceSplitInput = (split ' ' input)

isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween a b c = (a >= b) && (a <= c )

separateChar :: String -> Char -> [String]
separateChar [] _ = [""]
separateChar (x: xs) c  | x == c = ("" : (c : "") : separateChar xs c)
                    | otherwise = (x: head (separateChar xs c)) : tail (separateChar xs c)

separateCharOnList :: [String] -> Char -> [String]
separateCharOnList [] _ = []
separateCharOnList (x : xs) c = separateChar x c ++ separateCharOnList xs c

clean :: (Eq a) => [a] -> a -> [a]
clean xs match = filter (match /= ) xs

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

getInsideParentheses :: [String] -> [String]
getInsideParentheses str = init $ tail (getParenthesis str 0)
