import Data.Foldable (find)
import Data.Either (rights, lefts, isRight)

--
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- GLaDOS scraper Main file
--

data Position = Position { line :: Int, char :: Int } deriving (Show)

defaultPosition :: Position
defaultPosition = Position { line = 0, char = 0 }

moveCursor :: Position -> Bool -> Position
moveCursor current True = Position { line = line current + 1, char = char current }
moveCursor current False =  Position { line = line current, char = char current + 1 }

type ParserOutput a = Either (String, Position) (a, String, Position)
type Parser a = String -> Position -> ParserOutput a

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

validOrFirstError :: [ParserOutput a] -> Position -> ParserOutput a
validOrFirstError [] current = Left ("Empty list", current)
validOrFirstError list _
    | null (rights list) = Left (head (lefts list))
    | otherwise = Right (head (rights list))

parseMultipleInclusif :: [Parser a] -> Parser a
parseMultipleInclusif list string current = validOrFirstError (map (\x -> x string current) list) current

parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = parseMultipleInclusif [first, second]
