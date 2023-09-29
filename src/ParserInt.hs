--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseInt.hs
--

module ParserInt (module ParserInt) where

import Control.Applicative (Alternative ((<|>)))
import ParserChar (parseChar, parseDigit)
import ParserType (Parser (..))
import SyntaxParser (parseSome)

parseUInt :: Parser Int
parseUInt = read <$> parseSome parseDigit

parseNegInt :: Parser Int
parseNegInt = (* (-1)) <$> (parseChar '-' *> parseUInt)

parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
