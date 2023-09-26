import Data.Foldable (find)
import Data.Either (rights, lefts, isRight)
import Text.Read

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

newtype Parser a = Parser {
    runParser :: String -> Position -> Either (String, Position) (a, String, Position)
}

parseChar :: Char -> Parser Char
parseChar char = Parser (\string pos -> case string of
    ('\n':xs)
        | '\n' == char -> Right ('\n', xs, moveCursor pos True)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    (x:xs)
        | x == char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    _ -> Left ("Invalid char found", defaultPosition)
    )

parseAnyChar :: [Char] -> Parser Char
parseAnyChar char = Parser (\string pos -> case string of
    ('\n':xs)
        | '\n' `elem` char -> Right ('\n', xs, moveCursor pos True)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    (x:xs)
        | x `elem` char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    _ -> Left ("Invalid char found", defaultPosition)
    )

parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = Parser (\string pos -> case (runParser first string pos, runParser second string pos) of
    (Right (element, snd_string, snd_pos), Right _) -> Right (element, snd_string, snd_pos)
    (Right (element, snd_string, snd_pos), Left _) -> Right (element, snd_string, snd_pos)
    (Left _, Right (element, snd_string, snd_pos)) -> Right (element, snd_string, snd_pos)
    (Left a, Left _) -> Left a
    )

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd first second = Parser (\string pos -> case runParser first string pos of
    Right (element, snd_string, snd_pos) -> case runParser second snd_string snd_pos of
        Right (snd_element, thr_string, thr_pos) -> Right ((element, snd_element), thr_string, thr_pos)
        Left a -> Left a
    Left a -> Left a
    )

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fnct first second = Parser (\string pos -> case runParser (parseAnd first second) string pos of
    Right ((a, b), new_string, new_pos) -> Right (fnct a b, new_string, new_pos)
    Left a -> Left a
    )

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser (\string pos -> case runParser parse string pos of
    Right (element, new_string, new_pos) -> case runParser (parseMany parse) new_string new_pos of
        Left _ -> Right ([], new_string, new_pos)
        Right (found, found_string, found_pos) -> Right (element : found, found_string, found_pos)
    Left _ -> Right ([], string, pos)
    )

parseSome :: Parser a -> Parser [a]
parseSome parse = parseAndWith (:) parse (parseMany parse)

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseUInt :: Parser Int
parseUInt = Parser (\string pos -> case runParser (parseSome parseDigit) string pos of
    Right (found, found_string, found_pos) -> case readMaybe found of
        Nothing -> Left ( "Invalid digit found", pos )
        Just found_int -> Right ( found_int, found_string, found_pos )
    Left a -> Left a
    )

parseNegInt :: Parser Int
parseNegInt = Parser (\string pos -> case string of
    ('-':xs) -> case runParser (parseSome parseDigit) xs (moveCursor pos False) of
        Right (found, snd_string, snd_pos) -> case readMaybe ('-' : found) of
            Nothing -> Left ( "Invalid digit found", pos )
            Just found_int -> Right ( found_int, snd_string, snd_pos )
        Left a -> Left a
    _ -> Left ( "No neg sign found", pos )
    )

parseInt :: Parser Int
parseInt = parseOr parseNegInt parseUInt

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser = Parser (\string pos -> case runParser (parseMany (parseChar ' ')) string pos of
    Right (_, snd_string, snd_pos) -> case runParser parser snd_string snd_pos of
        Right (found, thr_string, thr_pos) -> case runParser (parseMany (parseChar ' ')) thr_string thr_pos  of
            Right (_, for_string, for_pos) -> Right (found, for_string, for_pos)
            Left a -> Left a
        Left a -> Left a
    Left a -> Left a
    )

parsePair :: Parser a -> Parser (a, a)
parsePair parser = Parser (\string pos -> case string of
    ('(':xs) -> case runParser (parseWithSpace parser) xs (moveCursor pos False) of
        Right (found, snd_string, snd_pos) -> case runParser (parseWithSpace parser) snd_string snd_pos of
            Right (snd_found, for_string, for_pos) -> case runParser (parseChar ')') for_string for_pos of
                Right (_, fif_string, fif_pos) -> Right ((found, snd_found), fif_string, fif_pos)
                Left (_, err_pos) -> Left ("Missing closing parenthesis", err_pos)
            Left a -> Left a
        Left a -> Left a
    _ -> Left ("Missing opening parenthesis", pos)
    )

{-
parseOrDiff :: Parser a -> Parser b -> Parser (Either a b)
parseOrDiff first second string pos = case (first string pos, second string pos) of
    (Right (element, snd_string, snd_pos), Right _) -> Right (Left element, snd_string, snd_pos)
    (Right (element, snd_string, snd_pos), Left _) -> Right (Left element, snd_string, snd_pos)
    (Left _, Right (element, snd_string, snd_pos)) -> Right (Right element, snd_string, snd_pos)
    (Left a, Left _) -> Left a

parseList :: Parser a -> Parser [ a ]
parseList parser ('(':xs) pos = case parseSome (parseWithSpace parser) xs (moveCursor pos False) of
    Right (found, for_string, for_pos) -> case parseChar ')' for_string for_pos of
        Right (_, fif_string, fif_pos) -> Right (found, fif_string, fif_pos)
        Left (_, err_pos) -> Left ("Missing closing parenthesis", err_pos)
    Left a -> Left a
parseList _ _ pos = Left ("Missing opening parenthesis", pos)

-}
