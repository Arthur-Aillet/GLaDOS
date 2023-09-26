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

newtype Parser a = Parser {
    runParser :: String -> Position -> Either (String, Position) (a, String, Position)
}

parseChar :: Char -> Parser Char
parseChar char = Parser (\string pos -> case string of 
    ('\n':xs)
        | '\n' == char -> Right ('\n', xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    (x:xs)
        | x == char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    _ -> Left ("Invalid char found", defaultPosition)
    )       

parseAnyChar :: [Char] -> Parser Char
parseAnyChar char = Parser (\string pos -> case string of 
    ('\n':xs)
        | '\n' `elem` char -> Right ('\n', xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    (x:xs)
        | x `elem` char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", defaultPosition)
    _ -> Left ("Invalid char found", defaultPosition)
    )       

{-
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
    Right (element, new_string, new_pos) -> case parseMany parse new_string new_pos of
        Left _ -> Right ([], new_string, new_pos)
        Right (found, found_string, found_pos) -> Right (element : found, found_string, found_pos)
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

parseInt :: Parser Int
parseInt ('-':xs) pos = case parseSome parseDigit xs (moveCursor pos False) of
    Right (found, snd_string, snd_pos) -> case readMaybe ('-' : found) of
        Nothing -> Left ( "Invalid digit found", pos )
        Just found_int -> Right ( found_int, snd_string, snd_pos )
    Left a -> Left a
parseInt string pos = case parseSome parseDigit string pos of
    Right (found, found_string, found_pos) -> case readMaybe found of
        Nothing -> Left ( "Invalid digit found", pos )
        Just found_int -> Right ( found_int, found_string, found_pos )
    Left a -> Left a

parsePair :: Parser a -> Parser (a, a)
parsePair parser ('(':xs) pos = case parseWithSpace parser xs (moveCursor pos False) of
    Right (found, snd_string, snd_pos) -> case parseWithSpace parser snd_string snd_pos of
        Right (snd_found, for_string, for_pos) -> case parseChar ')' for_string for_pos of
            Right (_, fif_string, fif_pos) -> Right ((found, snd_found), fif_string, fif_pos)
            Left (_, err_pos) -> Left ("Missing closing parenthesis", err_pos)
        Left a -> Left a
    Left a -> Left a
parsePair _ _ pos = Left ("Missing opening parenthesis", pos)

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser string pos = case parseMany (parseChar ' ') string pos of
    Right (_, snd_string, snd_pos) -> case parser snd_string snd_pos of
        Right (found, thr_string, thr_pos) -> case parseMany (parseChar ' ') thr_string thr_pos  of
            Right (_, for_string, for_pos) -> Right (found, for_string, for_pos)
            Left a -> Left a
        Left a -> Left a
    Left a -> Left a

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
