{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS Main file
-}

import PositionType (Position, moveCursor)
import ParserType (Parser(..))
import Control.Applicative (Alternative((<|>)))

main :: IO ()
main = putStrLn "Hello, World!"

withErr :: String -> Parser a -> Parser a
withErr message parser = Parser $ \string pos -> case runParser parser string pos of 
    Right a -> Right a
    Left (_, new_pos) -> Left (message, new_pos)

parseChar :: Char -> Parser Char
parseChar char = Parser $ \string pos -> case string of
    ('\n':xs)
        | '\n' == char -> Right ('\n', xs, moveCursor pos True)
        | otherwise -> Left ("Invalid char found", moveCursor pos True)
    (x:xs)
        | x == char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", moveCursor pos False)
    _ -> Left ("Invalid char found", pos)

parseAnyChar :: [Char] -> Parser Char
parseAnyChar char = Parser $ \string pos -> case string of
    ('\n':xs)
        | '\n' `elem` char -> Right ('\n', xs, moveCursor pos True)
        | otherwise -> Left ("Invalid char found", moveCursor pos True)
    (x:xs)
        | x `elem` char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", moveCursor pos False)
    _ -> Left ("Invalid char found", pos)

parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = first <|> second

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fnct first second = fnct <$> first <*> second

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser $ \string pos -> case runParser parse string pos of
    Right (element, new_string, new_pos) -> case runParser (parseMany parse) new_string new_pos of
        Left _ -> Right ([], new_string, new_pos)
        Right (found, found_string, found_pos) -> Right (element : found, found_string, found_pos)
    Left _ -> Right ([], string, pos)

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseUInt :: Parser Int
parseUInt = read <$> parseSome parseDigit

parseNegInt :: Parser Int
parseNegInt = (\_ x -> x * (-1)) <$> parseChar '-' <*> parseUInt

parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser = seq <$> parseMany (parseChar ' ')
    <*> (const <$> parser <*> parseMany (parseChar ' '))

parseOpeningParenthesis :: Parser Char
parseOpeningParenthesis = withErr "Missing opening parenthesis" (parseChar '(')

parseClosingParenthesis :: Parser Char
parseClosingParenthesis = withErr "Missing closing parenthesis" (parseChar ')')

parsePair :: Parser a -> Parser (a, a)
parsePair parser = parseWithSpace ((,) <$>
    (seq <$> parseOpeningParenthesis <*> parseWithSpace parser)
    <*> (const <$> parseWithSpace parser <*> parseClosingParenthesis))

parseList :: Parser a -> Parser [a]
parseList parser = parseWithSpace (const <$>
    (seq <$> parseOpeningParenthesis <*> parseMany (parseWithSpace parser))
    <*> parseClosingParenthesis)
