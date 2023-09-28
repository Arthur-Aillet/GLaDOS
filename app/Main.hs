{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS Main file
-}

import Control.Applicative (Alternative ((<|>)))
import ParserType (Parser (..))
import PositionType (moveCursor)
import Data.Bool (Bool)
import Data.Either

main :: IO ()
main = putStrLn "Hello, World!"

withErr :: String -> Parser a -> Parser a
withErr msg parser = Parser $ \string pos -> case runParser parser string pos of
  Right a -> Right a
  Left (_, new_pos) -> Left (msg, new_pos)

failingWith :: String -> Parser a
failingWith string = Parser (\_ pos -> Left (string, pos))

parseAChar :: Parser Char
parseAChar = Parser $ \string pos -> case string of
  ('\n' : xs) -> Right ('\n', xs, moveCursor pos True)
  (x : xs) ->  Right (x, xs, moveCursor pos False)
  [] -> Left ("Char not present in empty list", pos)

parseChar :: Char -> Parser Char
parseChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Right (char, new_str, new_pos)
    | otherwise -> Left ("Invalid char found", moveCursor pos False)
  Left (_, pos) -> Left ("Char not present in empty list", pos)

parseNotChar :: Char -> Parser Char
parseNotChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Left ("Invalid char found", moveCursor pos False)
    | otherwise -> Right (char, new_str, new_pos)
  Left (_, pos) -> Left ("Char not present in empty list", pos)

parseAnyChar :: [Char] -> Parser Char
parseAnyChar = foldl
  (\a b -> a <|> parseChar b)
  (failingWith "Char not found in list")

parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = first <|> second

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fnct first second = fnct <$> first <*> second

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser $ \string pos -> case runParser parse string pos of
  Right (element, new_str, new_pos) ->
    case runParser (parseMany parse) new_str new_pos of
      Left _ -> Right ([], new_str, new_pos)
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left _ -> Right ([], string, pos)

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0' .. '9']

parseBool :: Parser Bool
parseBool = (== 't') <$> (parseChar '#' *> parseAnyChar ['f', 't'])

parseSymbol :: String -> Parser String
parseSymbol string = Parser $ \str pos -> 
  case runParser (parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z']))) str pos of
    Right (found, s, p)
      | found == string -> Right (found, s, p) 
      | otherwise -> Left ("Invalid string found", p)
    Left (_, new_pos) -> Left ("String not found", new_pos)

parseOpeningQuote :: Parser Char
parseOpeningQuote = withErr "Missing opening Quote" (parseChar '"')

parseClosingQuote :: Parser Char
parseClosingQuote = withErr "Missing closing Quote" (parseChar '"')

parseString :: Parser String
parseString = parseOpeningQuote *> parseMany (parseNotChar '"') <* parseClosingQuote

parseUInt :: Parser Int
parseUInt = read <$> parseSome parseDigit

parseNegInt :: Parser Int
parseNegInt = (* (-1)) <$> (parseChar '-' *> parseUInt)

parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser =
  parseMany (parseChar ' ') *> parser <* parseMany (parseChar ' ')

parseOpeningParenthesis :: Parser Char
parseOpeningParenthesis = withErr "Missing opening parenthesis" (parseChar '(')

parseClosingParenthesis :: Parser Char
parseClosingParenthesis = withErr "Missing closing parenthesis" (parseChar ')')

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
    ( parseOpeningParenthesis *>
    parseMany (parseWithSpace parser) <*
    parseClosingParenthesis )
