module Parsing.Position
    ( Position(..)
    , new
    , nextLine
    , nextCol
    ) where

data Position = Position {
    line :: Int,
    col :: Int
} deriving (Eq, Show)

new :: Position
new = Position 0 0

nextLine :: Position -> Position
nextLine (Position l _) = Position (l + 1) 0

nextCol :: Position -> Position
nextCol (Position l c) = Position l (c + 1)
