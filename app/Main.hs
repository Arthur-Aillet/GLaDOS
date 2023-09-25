--
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
--

data Position = Position { line :: Int, char :: Int }

type Parser a = String -> Either (String, Position) (a, String, Position)

parseChar :: Char -> Parser Char
parseChar _ _ = Right ('a', "atest", Position { line = 1, char = 2 })
