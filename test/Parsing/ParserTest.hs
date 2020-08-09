module Parsing.ParserTest where

import Control.Applicative ((<|>))
import Data.Either (isLeft)
import Test.HUnit

import Parsing.Parser

suite :: Test
suite = TestLabel "Parser" (TestList
    [ pcharTest
    , pdigitTest
    , panycharTest
    , pstrTest
    , pquotedstrTest
    , enclosedTest
    , withErrorTest
    , altTest
    , tupleTest
    , ignoreTest
    , pfilterTest
    , optionTest
    , zeroOrMoreTest
    , oneOrMoreTest
    , anyOfTest
    , whitespaceTest
    , eofTest
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

panycharTest :: Test
panycharTest = TestLabel "panychar" (TestList
    [ TestCase $ assertEqual "exists" (Right ('a', "bc")) $ parse panychar "abc"
    , TestCase $ assertBool "empty" $ isLeft (parse panychar [])
    ])

pstrTest :: Test
pstrTest = TestLabel "pstr" (TestList
    [ TestCase $ assertEqual "match" (Right ("abc", "")) $ parse (pstr "abc") "abc"
    , TestCase $ assertBool "no match" $ isLeft (parse (pstr "bc") "abc")
    , TestCase $ assertBool "part match" $ isLeft (parse (pstr "acb") "abc")
    ])

pquotedstrTest :: Test
pquotedstrTest = TestLabel "pquotedstring" (TestList
    [ TestCase $ assertEqual "success" (Right ("value", "")) $ parse pquotedstr "\"value\""
    , TestCase $ assertBool "no open quote" $ isLeft (parse pquotedstr "value\"")
    , TestCase $ assertBool "no close quote" $ isLeft (parse pquotedstr "\"value")
    ])

enclosedTest :: Test
enclosedTest = TestLabel "enclosed" (TestList
    [ TestCase $ assertEqual "success" (Right ("value", "")) $ parse (enclosed (pchar '(') (pchar ')') pvalue) "(value)"
    , TestCase $ assertEqual "success2" (Right ("value", "more<>")) $ parse (enclosed (pchar '<') (pchar '>') pvalue) "<value>more<>"
    , TestCase $ assertBool "no open" $ isLeft (parse (enclosed (pchar '(') (pchar ')') pvalue) "value)")
    , TestCase $ assertBool "no close" $ isLeft (parse (enclosed (pchar '(') (pchar ')') pvalue) "(value")
    ])
  where
    pvalue = zeroOrMore (pfilter notEnclosingChar panychar)
    notEnclosingChar c = and $ (/=) c <$> ['(', ')', '<', '>']

withErrorTest :: Test
withErrorTest = TestLabel "withError" (TestList
    [ TestCase $ assertEqual "success unchanged" (Right ('a',"bc")) $ parse (pchar 'a' <?> "my error") "abc"
    , TestCase $ assertEqual "failure <?> msg" (Left "my error") $ parse (pchar 'a' <?> "my error") ""
    , TestCase $ assertEqual "withError msg failure" (Left "my error") $ parse (withError "my error" $ pchar 'a') ""
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

pfilterTest :: Test
pfilterTest = TestLabel "pfilter" (TestList
    [ TestCase $ assertEqual "pfilter true" (Right ('a', "bc")) $ parse (pfilter (== 'a') $ pchar 'a') "abc"
    , TestCase $ assertBool "pfilter false" $ isLeft (parse (pfilter (== 'b') $ pchar 'a') "abc")
    , TestCase $ assertBool "pfilter fail" $ isLeft (parse (pfilter (== 'a') $ pchar 'b') "abc")
    ])

optionTest :: Test
optionTest = TestLabel "option" (TestList
    [ TestCase $ assertEqual "match" (Right (Just 'a', "bc")) $ parse (option $ pchar 'a') "abc"
    , TestCase $ assertEqual "no match" (Right (Nothing, "abc")) $ parse (option $ pchar 'b') "abc"
    ])

oneOrMoreTest :: Test
oneOrMoreTest = TestLabel "oneOrMore" (TestList
    [ TestCase $ assertBool "empty" $ isLeft (parse (oneOrMore $ pchar 'a') "")
    , TestCase $ assertBool "none" $ isLeft (parse (oneOrMore $ pchar 'b') "abc")
    , TestCase $ assertEqual "one" (Right ("a", "bc")) $ parse (oneOrMore $ pchar 'a') "abc"
    , TestCase $ assertEqual "more" (Right ("aaa", "bc")) $ parse (oneOrMore $ pchar 'a') "aaabc"
    ])

zeroOrMoreTest :: Test
zeroOrMoreTest = TestLabel "zeroOrMore" (TestList
    [ TestCase $ assertEqual "empty" (Right ([], "")) $ parse (zeroOrMore $ pchar 'a') ""
    , TestCase $ assertEqual "none" (Right ([], "abc")) $ parse (zeroOrMore $ pchar 'b') "abc"
    , TestCase $ assertEqual "one" (Right ("a", "bc")) $ parse (zeroOrMore $ pchar 'a') "abc"
    , TestCase $ assertEqual "more" (Right ("aaa", "bc")) $ parse (zeroOrMore $ pchar 'a') "aaabc"
    ])

anyOfTest :: Test
anyOfTest = TestLabel "anyOf" (TestList
    [ TestCase $ assertEqual "match" (Right ('a', "bc")) $ parse (anyOf [pchar 'c', pchar 'b', pchar 'a']) "abc"
    , TestCase $ assertBool "no match" $ isLeft (parse (anyOf [pchar 'b', pchar 'c']) "abc")
    , TestCase $ assertBool "empty parsers" $ isLeft (parse (anyOf []) "abc")
    ])

whitespaceTest :: Test
whitespaceTest = TestLabel "whitespace" (TestList
    [ TestCase $ assertEqual "space" (Right (' ', "")) $ parse whitespace " "
    , TestCase $ assertEqual "tab" (Right ('\t', "")) $ parse whitespace "\t"
    , TestCase $ assertEqual "carriage return" (Right ('\r', "")) $ parse whitespace "\r"
    , TestCase $ assertEqual "new line" (Right ('\n', "")) $ parse whitespace "\n"
    , TestCase $ assertBool "empty" $ isLeft (parse whitespace "")
    , TestCase $ assertBool "non whitespace" $ isLeft (parse whitespace "a")
    ])

eofTest :: Test
eofTest = TestLabel "eof" (TestList
    [ TestCase $ assertEqual "true" (Right ((), "")) $ parse eof ""
    , TestCase $ assertBool "false" $ isLeft (parse eof "a")
    ])
