import Data.Foldable (find)

--
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
--

data Position = Position { line :: Int, char :: Int } deriving (Show)

moveCursor :: Position -> Bool -> Position
moveCursor current True = Position { line = line current + 1, char = char current }
moveCursor current False =  Position { line = line current, char = char current + 1 }

type Parser a = String -> Position -> Either (String, Position) (a, String, Position)

parseChar :: Char -> Parser Char
parseChar '\n' ('\n':xs) current = Right ('\n', xs, moveCursor current True)
parseChar char (x:xs) current
    | char == x = Right (x, xs, moveCursor current False)
    | otherwise = Left ( "Invalid char found", current )
parseChar _ _ current = Left ( "Invalid char found", current )

parseFoundChar :: Maybe Char -> Parser Char
parseFoundChar Nothing _ current = Left ( "Char not found", current )
parseFoundChar (Just '\n') string current = Right ('\n', string, moveCursor current True)
parseFoundChar (Just a) string current =  Right (a, string, moveCursor current False)

parseAnyChar :: String -> Parser Char
parseAnyChar "" _ current = Left ( "Empty list of chars", current )
parseAnyChar chars (x:xs) current = parseFoundChar (find (== x) chars) xs current
parseAnyChar _ _ current = Left ( "List empty", current )

