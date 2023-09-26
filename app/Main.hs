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

instance Functor Parser where
    fmap fct parser = Parser (\string pos -> case runParser parser string pos of
        Right (a, new_string, new_pos) -> Right (fct a, new_string, new_pos)
        Left a -> Left a
        )

instance Applicative Parser where
    pure a = Parser (\string pos -> Right (a, string, pos))
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) parserfct parsera = Parser (\string pos -> case runParser parserfct string pos of
        Right (fct, new_string, new_pos) -> case runParser parsera new_string new_pos of
            Right (a, snd_string, snd_pos) -> Right (fct a, snd_string, snd_pos)
            Left a -> Left a
        Left a -> Left a
        )

parseChar :: Char -> Parser Char
parseChar char = Parser (\string pos -> case string of
    ('\n':xs)
        | '\n' == char -> Right ('\n', xs, moveCursor pos True)
        | otherwise -> Left ("Invalid char found", moveCursor pos True)
    (x:xs)
        | x == char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", moveCursor pos False)
    _ -> Left ("Invalid char found", defaultPosition)
    )

parseAnyChar :: [Char] -> Parser Char
parseAnyChar char = Parser (\string pos -> case string of
    ('\n':xs)
        | '\n' `elem` char -> Right ('\n', xs, moveCursor pos True)
        | otherwise -> Left ("Invalid char found", moveCursor pos True)
    (x:xs)
        | x `elem` char -> Right (x, xs, moveCursor pos False)
        | otherwise -> Left ("Invalid char found", moveCursor pos False)
    _ -> Left ("Invalid char found", defaultPosition)
    )

parseOr :: Parser a -> Parser a -> Parser a
parseOr first second = Parser (\string pos -> case (runParser first string pos, runParser second string pos) of
    (Right (element, snd_string, snd_pos), Right _) -> Right (element, snd_string, snd_pos)
    (Right (element, snd_string, snd_pos), Left _) -> Right (element, snd_string, snd_pos)
    (Left _, Right (element, snd_string, snd_pos)) -> Right (element, snd_string, snd_pos)
    (Left a, Left _) -> Left a
    )

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fnct first second = fnct <$> first <*> second

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser (\string pos -> case runParser parse string pos of
    Right (element, new_string, new_pos) -> case runParser (parseMany parse) new_string new_pos of
        Left _ -> Right ([], new_string, new_pos)
        Right (found, found_string, found_pos) -> Right (element : found, found_string, found_pos)
    Left _ -> Right ([], string, pos)
    )

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseUInt :: Parser Int
parseUInt = read <$> parseSome parseDigit

parseNegInt :: Parser Int
parseNegInt = (\_ x -> x * (-1)) <$> parseChar '-' <*> parseUInt

parseInt :: Parser Int
parseInt = parseOr parseNegInt parseUInt

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser = seq <$> parseMany (parseChar ' ')
    <*> (const <$> parser <*> parseMany (parseChar ' '))

parsePair :: Parser a -> Parser (a, a)
parsePair parser = parseWithSpace ((,) <$>
    (seq <$> parseChar '(' <*> parseWithSpace parser)
    <*> (const <$> parseWithSpace parser <*> parseChar ')'))

parseList :: Parser a -> Parser [a]
parseList parser = parseWithSpace (const <$>
    (seq <$> parseChar '(' <*> parseMany (parseWithSpace parser))
    <*> parseChar ')')
