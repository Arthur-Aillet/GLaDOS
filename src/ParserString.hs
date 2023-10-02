--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseString.hs
--

module ParserString (module ParserString) where

import ParserChar (parseAnyChar, parseClosingQuote, parseNotChar, parseOpeningQuote)
import ParserType (Parser (..))
import SyntaxParser (parseMany, parseSome)

parseSymbol :: String -> Parser String
parseSymbol string = Parser $ \s p ->
  case runParser (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z']))) s p of
    Right (found, s, p)
      | found == string -> Right (found, s, p)
      | otherwise -> Left ("Invalid string found", p)
    Left (_, new_pos) -> Left ("String not found", new_pos)

parseString :: Parser String
parseString =
  parseOpeningQuote
    *> parseMany (parseNotChar '"')
    <* parseClosingQuote
