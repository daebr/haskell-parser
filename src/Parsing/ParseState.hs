module Parsing.ParseState
    ( ParseState(..)
    , Source
    , new
    , curLine
    , curCol
    ) where

import qualified Parsing.Position as Pos
import Parsing.Position (Position)

type Source = [String]

data ParseState = ParseState {
    source :: Source,
    position :: Position
} deriving (Eq, Show)

new :: Source -> ParseState
new source = ParseState source Pos.new

curLine :: ParseState -> Int
curLine = Pos.line . position

curCol :: ParseState -> Int
curCol = Pos.col . position
