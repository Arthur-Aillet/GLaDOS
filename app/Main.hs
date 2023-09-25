--
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
--

data Position = Position { line :: Int, char :: Int }

data Result a b = Ok a | Err b

type Parser a = String -> Result (a, String, Position) (String, Position)

