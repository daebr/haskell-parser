module Parsing.ParserTest where

import Data.Either (isLeft)
import Test.HUnit

import Parsing.Parser

suite :: Test
suite = TestLabel "Parser" (TestList
    [ pcharTest
    ])

pcharTest :: Test
pcharTest = TestLabel "pchar" (TestList
    [ TestCase $ assertEqual "match" (Right ('a', "bc")) (parse (pchar 'a') "abc")
    , TestCase $ assertBool "not match" $ isLeft (parse (pchar 'b') "abc")
    , TestCase $ assertBool "empty" $ isLeft (parse (pchar 'a') "")
    ])
