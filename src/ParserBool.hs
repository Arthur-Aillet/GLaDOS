--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- parseBool.hs
--

module ParserBool (module ParserBool) where

import ParserChar (parseAnyChar, parseChar)
import ParserType (Parser (..))

parseBool :: Parser Bool
parseBool = (== 't') <$> (parseChar '#' *> parseAnyChar ['f', 't'])
