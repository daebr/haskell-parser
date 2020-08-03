{-# LANGUAGE LambdaCase #-}

module Parsing.Parser where

import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)

type Parser a = StateT String (Either String) a
type ParseResult a = Either String a

lift :: (String -> ParseResult (a, String)) -> Parser a
lift = StateT 

parse :: Parser a -> String -> ParseResult (a, String)
parse = runStateT

pchar :: Parser Char
pchar = lift $ \case
    [] -> Left "empty"
    (x:xs) -> Right (x, xs)
