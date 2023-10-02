{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Parser SExpr
-}

module ParserSExpr (module ParserSExpr) where

import Control.Applicative (Alternative ((<|>)))
import ParserBool (parseBool)
import ParserChar (parseAnyChar)
import ParserInt (parseFloat, parseInt)
import ParserString (parseSym)
import ParserType (Parser (..))
import SParser (SExpr (..))
import SyntaxParser (parseList, parseSome)

parseSExpr :: Parser SExpr
parseSExpr =
  (SList <$> parseList parseSExpr)
    <|> (SInt <$> parseInt)
    <|> (SFloat <$> parseFloat)
    <|> (SBool <$> parseBool)
    <|> (SSym <$> parseSym)
