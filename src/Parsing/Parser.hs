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
    , pchar
    , pdigit
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

match :: (Char -> Bool) -> Parser Char
match f = lift $ \case
    [] -> Left "empty"
    (x:xs) | f x -> Right (x, xs)
           | otherwise -> Left "parse failed"

option :: Parser a -> Parser (Maybe a)
option p = Just <$> p <|> pure Nothing

pchar :: Char -> Parser Char
pchar = match . (==)

pdigit :: Parser Char
pdigit = match isDigit
