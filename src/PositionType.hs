{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module PositionType (Position, defaultPosition, moveCursor) where

data Position = Position { line :: Int, char :: Int } deriving (Show)

defaultPosition :: Position
defaultPosition = Position { line = 0, char = 0 }

moveCursor :: Position -> Bool -> Position
moveCursor current True = Position { line = line current + 1, char = char current }
moveCursor current False =  Position { line = line current, char = char current + 1 }
