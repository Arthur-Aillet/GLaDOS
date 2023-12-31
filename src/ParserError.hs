{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseError
-}

module ParserError (module ParserError) where

import ParserType (Parser (..))
import PositionType (Position (..))

withErr :: String -> Parser a -> Parser a
withErr msg parser = Parser $ \string pos -> case runParser parser string pos of
  Right a -> Right a
  Left (_, new_pos) -> Left (msg, new_pos)

failingWith :: String -> Parser a
failingWith string = Parser (\_ pos -> Left (string, pos))

printErr :: (String, Position) -> IO ()
printErr (err, pos) =
  putStrLn
    ( "Error found at "
        ++ show (line pos)
        ++ ":"
        ++ show (char pos)
        ++ ":"
        ++ "\n    "
        ++ err
    )
