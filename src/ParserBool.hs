--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- parseBool.hs
 --

module ParserBool (module ParserBool) where

import ParserType (Parser (..))
import ParserChar (parseAnyChar, parseChar)

parseBool :: Parser Bool
parseBool = (== 't') <$> (parseChar '#' *> parseAnyChar ['f', 't'])
