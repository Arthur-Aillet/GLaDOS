--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParserCombinator.hs
--

module ParserCombinator (module ParserCombinator) where

import Control.Applicative (Alternative ((<|>)))
import ParserType (Parser (..))

parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = first <|> second

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fnct first second = fnct <$> first <*> second

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)
