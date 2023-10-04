{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseString
-}

module ParserString (module ParserString) where

import ParserChar (parseAnyChar, parseClosingQuote, parseNotChar, parseOpeningQuote)
import ParserType (Parser (..))
import SyntaxParser (parseMany, parseSome)

parseSymbol :: String -> Parser String
parseSymbol string = Parser $ \s p ->
  case runParser (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z']))) s p of
    Right (found, n_s, n_p)
      | found == string -> Right (found, n_s, n_p)
      | otherwise -> Left ("Error: Symbols are not the same", p)
    Left (_, new_pos) -> Left ("Not Found: List is empty", new_pos)

parseString :: Parser String
parseString =
  parseOpeningQuote
    *> parseMany (parseNotChar '"')
    <* parseClosingQuote

parseSym :: Parser String
parseSym = parseSome (parseAnyChar (['!' .. '\''] ++ ['*' .. '~']))
