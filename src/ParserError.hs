--
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseError.hs
--

module ParserError (module ParserError) where

import ParserType (Parser (..))

withErr :: String -> Parser a -> Parser a
withErr msg parser = Parser $ \string pos -> case runParser parser string pos of
  Right a -> Right a
  Left (_, new_pos) -> Left (msg, new_pos)

failingWith :: String -> Parser a
failingWith string = Parser (\_ pos -> Left (string, pos))
