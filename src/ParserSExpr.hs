{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Parser SExpr
-}

module ParserSExpr (module ParserSExpr) where

import Control.Applicative (Alternative ((<|>)))

import ParserType (Parser (..))
import SParser (SExpr (..))
import SyntaxParser ( parseSome, parseList )
import ParserInt ( parseInt, parseFloat )
import ParserBool ( parseBool )
import ParserChar ( parseAnyChar )

parseSExpr :: Parser SExpr
parseSExpr =
    (SList <$> parseList parseSExpr)
    <|> (SInt <$> parseInt)
    <|> (SFloat <$> parseFloat)
    <|> (SBool <$> parseBool)
    <|> (SSym <$> parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['*'..'/'])))
