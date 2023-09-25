--
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
--

data Position = Position { line :: Int, char :: Int }

moveCursor :: Position -> Bool -> Position
moveCursor current True = Position { line = line current + 1, char = char current }
moveCursor current False =  Position { line = line current, char = char current + 1 }

type Parser a = String -> Position -> Either (String, Position) (a, String, Position)

parseChar :: Char -> Parser Char
parseChar '\n' ('\n':_) current = Right ('a', "atest", moveCursor current True)
parseChar char (x:xs) current
    | char == x = Right (x, xs, moveCursor current False)
    | otherwise = Left ( "Invalid char found", current )
parseChar _ _ current = Left ( "Invalid char found", current )
