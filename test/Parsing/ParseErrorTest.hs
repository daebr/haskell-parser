{-# LANGUAGE OverloadedStrings #-}

module Parsing.ParseErrorTest where

import Data.Functor ((<&>))
import Test.HUnit
import Text.Regex.PCRE

import Parsing.Position (Position(..))
import Parsing.ParseError

suite :: Test
suite = TestLabel "ParseError" (TestList
    [ showTest
    ])

showTest :: Test
showTest = TestLabel "show" (TestList
    [ TestLabel "empty line"
        ( TestList (
            ( $ mkError "error message" "" (0, 0))
              <$> [ expect "message" "error message"
                  , expect "line" ("line 1")
                  , expect "col" ("col 1")
                  ])
        )
    , TestLabel "short line"
        ( TestList (
            ( $ mkError "error message" "short line" (1, 3))
              <$> [ expect "message" "error message"
                  , expect "line" ("line 2")
                  , expect "col" ("col 4")
                  , expect "pos indicator" "short line\n\
                                           \   \\^"
                  ])
        )
    , TestLabel "start of line"
        ( TestList (
            ( $ mkError "error message" "start" (5, 0))
              <$> [ expect "pos indicator" "start\n\
                                           \\\^" ])
        )
    , TestLabel "start of long line"
        ( TestList (
            ( $ mkError "error message" "the quick brown fox jumps over the lazy dog" (0, 4))
              <$> [ expect "pos indicator" "the quick brow...\n\
                                           \    \\^" ])
        ) 
    , TestLabel "mid of long line"
        ( TestList (
            ( $ mkError "error message" "the quick brown fox jumps over the lazy dog" (0, 15))
              <$> [ expect "pos indicator" "...uick brown fox jumps...\n\
                                           \            \\^" ])
        )
    , TestLabel "end of long line"
        ( TestList (
            ( $ mkError "error message" "the quick brown fox jumps over the lazy dog" (0, 40))
              <$> [ expect "pos indicator" "...the lazy dog\n\
                                           \            \\^" ])
        )
    ])
  where
    expect :: String -> String -> ParseError -> Test
    expect name text err =
        case show err =~ text of
            True -> TestCase (assertBool name True)
            False -> TestCase (assertEqual name ("contains " <> text) $ show err)

mkError :: String -> String -> (Int, Int) -> ParseError
mkError msg src (l, c) = ParseError msg src $ Position l c
