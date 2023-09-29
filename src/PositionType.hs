{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module PositionType (Position(..), defaultPosition, moveCursor) where

data Position = Position {line :: Int, char :: Int} deriving (Show, Eq)

defaultPosition :: Position
defaultPosition = Position {line = 0, char = 0}

moveCursor :: Position -> Bool -> Position
moveCursor pos True = Position {line = line pos + 1, char = char pos}
moveCursor pos False = Position {line = line pos, char = char pos + 1}
