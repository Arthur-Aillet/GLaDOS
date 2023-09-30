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
import SyntaxParser (parseSome, parseMany)

parseStringInt :: Parser String
parseStringInt = parseSome parseDigit

parseUInt :: Parser Int
parseUInt = read <$> parseStringInt

parseNegInt :: Parser Int
parseNegInt = (* (-1)) <$> (parseChar '-' *> parseUInt)

parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt

parseFloat :: Parser Float
parseFloat = (\intPart charPart -> read (intPart ++ charPart))
            <$> parseStringInt <*> parseFractionalPart

parseFractionalPart :: Parser String
parseFractionalPart = ('.' :) <$> (parseChar '.' *> parseMany parseDigit)