{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Parsing.Parser
    ( Parser
    , ParseResult
    , (.&&.)
    , (.&&)
    , (&&.)
    , (<?>)
    , parse
    , parseString
    , withError
    , option
    , zeroOrMore
    , oneOrMore
    , anyOf
    , pfilter
    , pchar
    , pdigit
    , pstr
    , pquotedstr
    , whitespace
    , eof
    ) where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)

import Parsing.ParseError (ParseError(..))
import qualified Parsing.ParseState as ParseState
import Parsing.ParseState (ParseState(..), Source, curLine, curCol)
import Parsing.Position (Position)
import qualified Parsing.Position as Pos

type Parser a = StateT ParseState (Either ParseError) a
type ParseResult a = Either ParseError (a, ParseState)

instance Alternative (Either ParseError) where
    empty = Left (ParseError "Unknown failure" "" Pos.new)

    (Right a) <|> _ = Right a
    _ <|> (Right a) = Right a
    Left e <|> _ = Left e

instance MonadPlus (Either ParseError)

lift :: (ParseState -> ParseResult a) -> Parser a
lift = StateT 

parse :: Parser a -> Source -> ParseResult a
parse p = runStateT p . ParseState.new

parseString :: Parser a -> String -> ParseResult a
parseString p = parse p . lines

toError :: String -> ParseState -> ParseError
toError msg s = ParseError msg curLine (position s)
  where
    curLine = case source s of
        [] -> ""
        (x:_) -> x

next :: Parser Char
next = lift doNext
  where
    doNext :: ParseState -> ParseResult Char
    doNext s = case source s of
        [] -> Left $ toError "EOF" s
        (x:xs) | (curCol s) >= length x -> Right ('\n', s { source=xs, position=(Pos.nextLine $ position s) })
               | otherwise -> Right(x !! curCol s, s { position=(Pos.nextCol $ position s) })

failWith :: String -> Parser a
failWith s = lift $ Left . toError s 

withError :: String -> Parser a -> Parser a
withError msg p = lift $ \s ->
    case runStateT p s of
        Left err -> Left $ err { message=msg }
        Right a -> Right a 

infixl 3 <?>
(<?>) :: Parser a -> String -> Parser a
p <?> s = withError s p

infixl 5 .&&.
(.&&.) :: Parser a -> Parser b -> Parser (a, b)
pa .&&. pb = (,) <$> pa <*> pb

infixl 5 .&&
(.&&) :: Parser a -> Parser b -> Parser a
pa .&& pb = fst <$> pa .&&. pb

infixl 5 &&.
(&&.) :: Parser a -> Parser b -> Parser b
pa &&. pb = snd <$> pa .&&. pb

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (oneOrMore p <|> pure [])

anyOf :: [Parser a] -> Parser a
anyOf = foldl (<|>) (failWith "No parser satisfied")

match :: (Char -> Bool) -> Parser Char
match f = next >>= \c ->
    if f c
    then pure c
    else failWith "Failed to match Char"

pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p = lift $ \s ->
    case runStateT p s of
        Right (a, s') | f a -> Right (a, s')
                      | otherwise ->  Left $ toError "Failed to satisfy filter" s
        Left e -> Left e

option :: Parser a -> Parser (Maybe a)
option p = Just <$> p <|> pure Nothing

eof :: Parser ()
eof = lift isEof
  where
    isEof :: ParseState -> ParseResult ()
    isEof s = case source s of
        [] -> Right ((), s)
        _ -> Left $ toError "Expected EOF" s        

pchar :: Char -> Parser Char
pchar c = match (== c) <?> "Expected '" <> show c <> "'"

pdigit :: Parser Char
pdigit = match isDigit <?> "Expected digit"

whitespace :: Parser Char
whitespace = match (`elem` [' ', '\t', '\r', '\n']) <?> "Expected whitespace"

pstr :: String -> Parser String
pstr s = traverse pchar s <?> "Expected '" <> s <> "'"

pquotedstr :: Parser String
pquotedstr = quote &&. zeroOrMore nonQuote .&& quote <?> "Expected quoted string"
  where
    quote = pchar '"'
    nonQuote = match (/= '"')
