--
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
--

data Result a = Ok a | Err String

type Parser a = String -> Result (String, a)

