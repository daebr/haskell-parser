module Parsing.ParseState ( ParseState(..), Source ) where

import Parsing.Position

type Source = [String]

data ParseState = ParseState {
    source :: Source,
    position :: Position
}
