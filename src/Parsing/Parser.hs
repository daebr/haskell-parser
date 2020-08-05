{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Parsing.Parser
    ( Parser
    , ParseResult
    , (.&&.)
    , (.&&)
    , (&&.)
    , parse
    , option
    , zeroOrMore
    , oneOrMore
    , anyOf
    , pwhen
    , pchar
    , pdigit
    , panychar
    , pstr
    , pquotedstr
    , enclosed
    ) where

import Data.Char (isDigit)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)

type Parser a = StateT String (Either String) a
type ParseResult a = Either String a

instance Alternative (Either String) where
    empty = Left "no alternative match"
    (Right a) <|> _ = Right a
    _ <|> (Right a) = Right a
    _ <|> _ = empty

instance MonadPlus (Either String)

lift :: (String -> ParseResult (a, String)) -> Parser a
lift = StateT 

parse :: Parser a -> String -> ParseResult (a, String)
parse = runStateT

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
anyOf = foldl (<|>) (failWith "anyOf failed")

match :: (Char -> Bool) -> Parser Char
match f = lift $ \case
    [] -> Left "empty"
    (x:xs) | f x -> Right (x, xs)
           | otherwise -> Left "parse failed"

pwhen :: (a -> Bool) -> Parser a -> Parser a
pwhen f p = p >>= \a -> if f a
                        then pure a
                        else failWith "pwhen failed"

option :: Parser a -> Parser (Maybe a)
option p = Just <$> p <|> pure Nothing

enclosed :: Parser b -> Parser b -> Parser a -> Parser a
enclosed pstart pend p = pstart &&. p .&& pend

failWith :: String -> Parser a
failWith = lift . const . Left

pchar :: Char -> Parser Char
pchar = match . (==)

pdigit :: Parser Char
pdigit = match isDigit

panychar :: Parser Char
panychar = match $ const True

pstr :: String -> Parser String
pstr = traverse pchar

pquotedstr :: Parser String
pquotedstr = quote &&. zeroOrMore nonQuote .&& quote
  where
    quote = pchar '"'
    nonQuote = match (/= '"')
