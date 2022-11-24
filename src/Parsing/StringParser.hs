module Parsing.StringParser
    ( module P
    , QuotedStringError(..)
    , StringParser
    , eof
    , eol
    , pchar
    , pchari
    , pdigit
    , pint
    , pquotedstr
    , pstr
    , pstri
    , pwhitespace
    ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.Functor (void)
import Data.Void (absurd)

import Parsing.Parser ((<||>), (<?>))
import Parsing.Parser as P

type StringParser = P.Parser String

pwhitespace :: StringParser P.MatchError ()
pwhitespace = void $ P.match isSpace

pchar :: Char -> StringParser P.MatchError Char
pchar = P.match . (==)

pchari :: Char -> StringParser P.MatchError Char
pchari c = P.match $ \c' -> toLower c == toLower c'

pstr :: String -> StringParser P.MatchError String
pstr = traverse pchar

pstri :: String -> StringParser P.MatchError String
pstri = traverse pchari

data QuotedStringError
    = OpeningQuoteExpected
    | ClosingQuoteExpected
  deriving
    (Eq, Show)

pquotedstr :: StringParser QuotedStringError String
pquotedstr = openingQuote *> innerString <* closingQuote
  where
    openingQuote = setError OpeningQuoteExpected $ pchar '"'
    closingQuote = setError ClosingQuoteExpected $ pchar '"'
    innerString = P.zeroOrMore (P.match (/= '"')) <?> absurd

pdigit :: StringParser P.MatchError Char
pdigit = P.match isDigit

pint :: StringParser P.MatchError Int
pint = negOrPos <*> num
  where
    negOrPos = fmap (const negate) (pchar '-') <||> pure id
    num = read <$> oneOrMore pdigit

eol :: StringParser P.MatchError ()
eol = void (pstr "\r\n") <||> void (P.pelem ['\r', '\n'])

eof :: StringParser P.EndOfInputError ()
eof = P.pend
