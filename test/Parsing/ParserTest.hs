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
    [ TestCase $ assertEqual "non-empty" (Right ('a', "bc")) (parse pchar "abc")
    , TestCase $ assertBool "empty" $ isLeft (parse pchar "")
    ])
