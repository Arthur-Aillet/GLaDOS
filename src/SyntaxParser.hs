{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- SyntaxParser
-}

module SyntaxParser (module SyntaxParser) where

import ParserChar (parseChar, parseClosingParenthesis, parseOpeningParenthesis)
import ParserType (Parser (..))

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser $ \string pos -> case runParser parse string pos of
  Right (element, new_str, new_pos) ->
    case runParser (parseMany parse) new_str new_pos of
      Left _ -> Right ([], new_str, new_pos)
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left _ -> Right ([], string, pos)

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser =
  parseMany (parseChar ' ') *> parser <* parseMany (parseChar ' ')

parsePair :: Parser a -> Parser (a, a)
parsePair parser =
  parseWithSpace
    ( (,)
        <$> (parseOpeningParenthesis *> parseWithSpace parser)
        <*> (parseWithSpace parser <* parseClosingParenthesis)
    )

parseList :: Parser a -> Parser [a]
parseList parser =
  parseWithSpace
    ( parseOpeningParenthesis
        *> parseMany (parseWithSpace parser)
        <* parseClosingParenthesis
    )
