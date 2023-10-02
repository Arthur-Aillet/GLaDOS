{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseChar
-}

module ParserChar (module ParserChar) where

import Control.Applicative (Alternative ((<|>)))
import ParserError (failingWith, withErr)
import ParserType (Parser (..))
import PositionType (moveCursor)

parseAChar :: Parser Char
parseAChar = Parser $ \string pos -> case string of
  ('\n' : xs) -> Right ('\n', xs, moveCursor pos True)
  (x : xs) -> Right (x, xs, moveCursor pos False)
  [] -> Left ("Char not present in empty list", pos)

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0' .. '9']

parseOpeningQuote :: Parser Char
parseOpeningQuote = withErr "Missing opening Quote" (parseChar '"')

parseClosingQuote :: Parser Char
parseClosingQuote = withErr "Missing closing Quote" (parseChar '"')

parseOpeningParenthesis :: Parser Char
parseOpeningParenthesis = withErr "Missing opening parenthesis" (parseChar '(')

parseClosingParenthesis :: Parser Char
parseClosingParenthesis = withErr "Missing closing parenthesis" (parseChar ')')

parseChar :: Char -> Parser Char
parseChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Right (char, new_str, new_pos)
    | otherwise -> Left ("Invalid char found", moveCursor pos False)
  Left (_, new_pos) -> Left ("Char not present in empty list", new_pos)

parseNotChar :: Char -> Parser Char
parseNotChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Left ("Invalid char found", moveCursor pos False)
    | otherwise -> Right (char, new_str, new_pos)
  Left (_, new_pos) -> Left ("Char not present in empty list", new_pos)

parseAnyChar :: [Char] -> Parser Char
parseAnyChar =
  foldl
    (\a b -> a <|> parseChar b)
    (failingWith "Char not found in list")
