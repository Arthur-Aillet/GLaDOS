{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Parser type
-}

module ParserType where

import Control.Applicative (Alternative (empty, (<|>)))
import PositionType (Position)

newtype Parser a = Parser
  { runParser :: String -> Position -> Either (String, Position) (a, String, Position)
  }

instance Functor Parser where
  fmap fct parser = Parser $ \string pos -> case runParser parser string pos of
    Right (a, new_string, new_pos) -> Right (fct a, new_string, new_pos)
    Left a -> Left a

instance Applicative Parser where
  pure a = Parser $ \string pos -> Right (a, string, pos)
  (<*>) parserfct parsera = Parser $ \string pos -> case runParser parserfct string pos of
    Right (fct, new_string, new_pos) -> case runParser parsera new_string new_pos of
      Right (a, snd_string, snd_pos) -> Right (fct a, snd_string, snd_pos)
      Left a -> Left a
    Left a -> Left a
  a *> b = Parser $ \string pos -> case runParser a string pos of
    Right (_, new_string, new_pos) -> runParser b new_string new_pos
    Left _ -> runParser b string pos

instance Alternative Parser where
  empty = Parser $ \_ pos -> Left ("Empty", pos)
  first <|> second =
    Parser
      ( \string pos -> case (runParser first string pos, runParser second string pos) of
          (Right (element, snd_string, snd_pos), Right _) -> Right (element, snd_string, snd_pos)
          (Right (element, snd_string, snd_pos), Left _) -> Right (element, snd_string, snd_pos)
          (Left _, Right (element, snd_string, snd_pos)) -> Right (element, snd_string, snd_pos)
          (Left a, Left _) -> Left a
      )

instance Monad Parser where
  (>>) = (*>)
  a >>= fct = Parser $ \string pos -> case runParser a string pos of
    Right (res, new_string, new_pos) -> case runParser (fct res) new_string new_pos of
      Right (new, snd_string, snd_pos) -> Right (new, snd_string, snd_pos)
      Left err -> Left err
    Left err -> Left err
  return = pure
