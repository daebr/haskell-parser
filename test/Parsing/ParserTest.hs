module Parsing.ParserTest where

import Control.Applicative ((<|>))
import Data.Either (isLeft)
import Test.HUnit

import Parsing.Parser

suite :: Test
suite = TestLabel "Parser" (TestList
    [ pcharTest
    , pdigitTest
    , altTest
    , tupleTest
    , ignoreTest
    , optionTest
    ])

pcharTest :: Test
pcharTest = TestLabel "pchar" (TestList
    [ TestCase $ assertEqual "match" (Right ('a', "bc")) (parse (pchar 'a') "abc")
    , TestCase $ assertBool "not match" $ isLeft (parse (pchar 'b') "abc")
    , TestCase $ assertBool "empty" $ isLeft (parse (pchar 'a') "")
    ])

pdigitTest :: Test
pdigitTest = TestLabel "pdigit" (TestList
    [ TestCase $ assertEqual "match" (Right ('1', "")) $ parse pdigit "1"
    , TestCase $ assertBool "not match" $ isLeft (parse pdigit "a")
    ])

altTest :: Test
altTest = TestLabel "<|>" (TestList
    [ TestCase $ assertEqual "Right <|> Left" (Right ('a', "bc")) $ parse (pchar 'a' <|> pchar 'b') "abc"
    , TestCase $ assertEqual "Left <|> Right" (Right ('b', "c")) $ parse (pchar 'a' <|> pchar 'b') "bc"
    , TestCase $ assertBool "Left <|> Left" $ isLeft (parse (pchar 'a' <|> pchar 'b') "c")
    ])

tupleTest :: Test
tupleTest = TestLabel ".&&." (TestList
    [ TestCase $ assertEqual "success" (Right (('a','b'),"c")) $ parse (pchar 'a' .&&. pchar 'b') "abc"
    , TestCase $ assertBool "left fail" $ isLeft (parse (pchar 'a' .&&. pchar 'b') "bab")
    , TestCase $ assertBool "right fail" $ isLeft (parse (pchar 'a' .&&. pchar 'b') "acb")
    , TestCase $ assertBool "empty" $ isLeft (parse (pchar 'a' .&&. pchar 'b') "a")
    ])

ignoreTest :: Test
ignoreTest = TestLabel "ignore" (TestList
    [ TestCase $ assertEqual "a &&. b" (Right ('b', "c")) $ parse (pchar 'a' &&. pchar 'b') "abc"
    , TestCase $ assertEqual "a .&& b" (Right ('a', "c")) $ parse (pchar 'a' .&& pchar 'b') "abc"
    , TestCase $ assertBool "! &&. b" $ isLeft (parse (pchar 'b' &&. pchar 'a') "abc")
    , TestCase $ assertBool "a .&& !" $ isLeft (parse (pchar 'a' .&& pchar 'c') "abc")
    ])

optionTest :: Test
optionTest = TestLabel "option" (TestList
    [ TestCase $ assertEqual "match" (Right (Just 'a', "bc")) $ parse (option $ pchar 'a') "abc"
    , TestCase $ assertEqual "no match" (Right (Nothing, "abc")) $ parse (option $ pchar 'b') "abc"
    ])
