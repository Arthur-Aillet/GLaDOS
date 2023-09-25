import Data.Foldable (find)
import Data.Either (rights, lefts, isRight)
import Text.Read
import Data.Char (isDigit)

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

allValidOrFirstError :: [ParserOutput a] -> Position -> ParserOutput a
allValidOrFirstError [] current = Left ("Empty list", current)
allValidOrFirstError list _
    | all isRight list = head list
    | otherwise = Left (head (lefts list))

parseMultipleExclusif :: [Parser a] -> Parser a
parseMultipleExclusif list string current = allValidOrFirstError (map (\x -> x string current) list) current

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd first second string pos = case first string pos of
    Left a -> Left a
    Right (element, new_string, new_pos) -> case second new_string new_pos of
        Left a -> Left a
        Right (snd_elem, final_string, final_pos) -> Right ((element, snd_elem), final_string, final_pos)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fnct first second string pos = case parseAnd first second string pos of
    Left a -> Left a
    Right ((a, b), new_string, new_pos) -> Right (fnct a b, new_string, new_pos)

parseMany :: Parser a -> Parser [a]
parseMany parse string pos = case parse string pos of
    Right (elem, new_string, new_pos) -> case parseMany parse new_string new_pos of
        Left _ -> Right ([], new_string, new_pos)
        Right (found, found_string, found_pos) -> Right (found ++ [elem], found_string, found_pos)
    Left _ -> Right ([], string, pos)

parseSome :: Parser a -> Parser [a]
parseSome parse string pos = case parse string pos of
    Right (element, new_string, new_pos) -> case parseMany parse new_string new_pos of
        Left _ -> Right ([], new_string, new_pos)
        Right (found, found_string, found_pos) -> Right (element : found, found_string, found_pos)
    Left a -> Left a

parseDigit :: Parser Char
parseDigit (x:xs) current
    | isDigit x = Right (x, xs, moveCursor current False)
    | otherwise = Left ( "Invalid digit found", current )
parseDigit _ current = Left ( "Invalid digit found", current )

parseUInt :: Parser Int
parseUInt string pos = case parseSome parseDigit string pos of
    Right (found, found_string, found_pos) -> case readMaybe found of
        Nothing -> Left ( "Invalid digit found", pos )
        Just found_int -> Right ( found_int, found_string, found_pos )
    Left a -> Left a
