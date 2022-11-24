{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Parsing.Parser
    ( Parser()
    , EndOfInputError(..)
    , MatchError(..)
    , (<?>)
    , (<||>)
    , anyOf
    , anyOfNE
    , eval
    , exec
    , failIf
    , failure
    , filterWith
    , lift
    , mapError
    , match
    , oneOrMore
    , option
    , orElse
    , parse
    , pelem
    , pend
    , run
    , setError
    , zeroOrMore
    ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust)
import Data.Void (Void)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)

import Parsing.ParseError (ParseError(..))
import qualified Parsing.ParseState as ParseState
import Parsing.ParseState (ParseState(..), Source, curLine, curCol)
import Parsing.Position (Position)
import qualified Parsing.Position as Pos

newtype Parser i e a = Parser { runParse :: i -> Either e (a, i) }

lift :: (i -> Either e (a, i)) -> Parser i e a
lift = Parser

run :: Parser i e a -> i -> Either e (a, i)
run = runParse

parse :: Parser i e a -> i -> Either e a
parse p = fmap fst . run p

exec :: Parser i e a -> i -> Either e a
exec = parse

eval :: Parser i e a -> i -> Either e i
eval p = fmap snd . run p

instance Functor (Parser i e) where
    fmap f p = lift $ fmap (first f) . run p

instance Bifunctor (Parser i) where
    bimap f g p = lift $ bimap f (first g) . run p

instance Applicative (Parser i e) where
    pure a = lift $ \i -> Right (a, i)
    pf <*> pa = lift $ \i ->
        case run pf i of
            Right (f, i') -> first f <$> run pa i'
            Left e -> Left e

instance Monad (Parser i e) where
    pa >>= f = lift $ \i ->
        case run pa i of
            Right (a, i') -> run (f a) i'
            Left e -> Left e

failure :: e -> Parser i e a
failure e = lift (const $ Left e)

failIf :: (a -> e) -> (a -> Bool) -> Parser i e a -> Parser i e a
failIf fe f p = lift $ (>>= handle) . run p
  where
    handle (a,i) 
        | f a = Right (a,i)
        | otherwise = Left $ fe a

mapError :: (e -> e') -> Parser i e a -> Parser i e' a
mapError f p = lift $ first f . run p

setError :: e' -> Parser i e a -> Parser i e' a
setError e = mapError $ const e

infixl 3 <?>
(<?>) :: Parser i e a -> (e -> e') -> Parser i e' a
(<?>) = flip mapError

data MatchError
    = NoInput
    | MatchFailure
  deriving
    (Eq, Show)

match :: (a -> Bool) -> Parser [a] MatchError a
match f = lift $ \i ->
    case i of
        [] -> Left NoInput
        (x:xs)
            | f x -> Right (x, xs)
            | otherwise -> Left MatchFailure

data EndOfInputError = ExpectedEndOfInput deriving (Eq, Show)

pend :: Parser [a] EndOfInputError ()
pend = lift p
  where
    p [] = Right ((), [])
    p xs = Left ExpectedEndOfInput

orElse :: Parser i e a -> Parser i e a -> Parser i e a
orElse p1 p2 = lift $ \input ->
    case run p1 input of
        Right a -> Right a
        Left e -> case run p2 input of
            Right a -> Right a
            Left _ -> Left e

infixl 3 <||>
(<||>) = orElse

anyOf :: e' -> [Parser i e a] -> Parser i e' a
anyOf err = foldl (<||>) (failure err) . fmap (setError err)

anyOfNE :: NonEmpty (Parser i e a) -> Parser i e a
anyOfNE (x:|xs) = foldl (<||>) x xs

filterWith :: (a -> e) -> (a -> Bool) -> Parser i e a -> Parser i e a
filterWith fail f p = lift $ \input ->
    case run p input of
        Right (a, input')
            | f a -> Right (a, input')
            | otherwise -> Left (fail a)
        Left e -> Left e

zeroOrMore :: Parser i e a -> Parser i Void [a]
zeroOrMore p = lift $ \input ->
    case run (oneOrMore p) input of
        Right a -> Right a
        Left e -> Right ([], input)

oneOrMore :: Parser i e a -> Parser i e [a]
oneOrMore p = (:) <$> p <*> (oneOrMore p <||> pure [])

pelem :: Eq a => [a] -> Parser [a] MatchError a
pelem xs = match (`elem` xs)

option :: Parser i e a -> Parser i Void (Maybe a)
option p = lift $ \input ->
    case run p input of
        Right r -> Right $ first Just r
        Left _ -> Right (Nothing, input)
