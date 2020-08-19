{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Parsing.Parser
    ( Parser
    , ParseResult
    , (<|>)
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
    , zeroOrOne
    , anyOf
    , pfilter
    , pchar
    , pcharIn
    , pcharNotIn
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

next :: (Char -> Bool) -> Parser Char
next f = lift doNext
  where
    doNext :: ParseState -> ParseResult Char
    doNext s = case source s of
        [] -> Left $ toError "EOF" s
        (x:xs) | (curCol s) >= length x -> satisfy '\n' $ s { source=xs, position=(Pos.nextLine $ position s) }
               | otherwise -> satisfy (x !! curCol s) $ s { position=(Pos.nextCol $ position s) }
     where
        satisfy c s' =
            if f c
            then Right (c, s')
            else Left $ toError ("Unexpected " <> show c) s

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

zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne = option

anyOf :: [Parser a] -> Parser a
anyOf = foldl (<|>) (failWith "No parser satisfied")

pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p = lift $ \s ->
    case runStateT p s of
        Right (a, s') | f a -> Right (a, s')
                      | otherwise ->  Left $ toError "Failed to satisfy filter" s
        Left e -> Left e

match :: (Char -> Bool) -> Parser Char
match = next

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

pcharIn :: [Char] -> Parser Char
pcharIn = match . flip elem

pcharNotIn :: [Char] -> Parser Char
pcharNotIn cs = match $ not . flip elem cs

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
