{-# LANGUAGE LambdaCase #-}

module Parsing.Parser
    ( Parser
    , ParseResult
    , parse
    , pchar
    ) where

import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)

type Parser a = StateT String (Either String) a
type ParseResult a = Either String a

lift :: (String -> ParseResult (a, String)) -> Parser a
lift = StateT 

parse :: Parser a -> String -> ParseResult (a, String)
parse = runStateT

match :: (Char -> Bool) -> Parser Char
match f = lift $ \case
    [] -> Left "empty"
    (x:xs) | f x -> Right (x, xs)
           | otherwise -> Left "parse failed"

pchar :: Char -> Parser Char
pchar = match . (==)
