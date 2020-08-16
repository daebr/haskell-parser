module Parsing.ParserTest where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Test.HUnit

import Parsing.ParseError (ParseError(..))
import Parsing.ParseState (ParseState(..))
import Parsing.Parser

suite :: Test
suite = TestLabel "Parser" (TestList
    [ parseTest
    , parseStringTest
    , pcharTest
    , pcharInTest
    , pcharNotInTest
    , pdigitTest
    , pstrTest
    , pquotedstrTest
    , withErrorTest
    , altTest
    , combinerTest
    , pfilterTest
    , optionTest
    , zeroOrMoreTest
    , oneOrMoreTest
    , anyOfTest
    , whitespaceTest
    , eofTest
    ])

punit :: Parser ()
punit = pure ()

testSuccess :: (Eq a, Show a) => String -> a -> ParseResult a -> Test
testSuccess name expected actual = TestLabel name (TestCase $ assertEqual "value" (Right expected) $ fst <$> actual)

testFailure :: String -> ParseResult a -> Test
testFailure name actual = TestLabel name (TestCase $ assertBool "isLeft" $ isLeft actual)

parseTest :: Test
parseTest = TestLabel "parse" (TestList
    [ testSuccess "simple" () $ parse punit [""]
    , testSuccess "across lines" "a\nb" $ parse (pstr "a\nb") ["a","b","c"]
    , testSuccess "inline newline" "a\nb" $ parse (pstr "a\nb") ["a\nb\nc"]
    , testSuccess "empty" () $ parse punit []
    , testSuccess "emptyLine" '\n' $ parse (pchar '\n') [""]
    ]) 

parseStringTest :: Test
parseStringTest = TestLabel "parseString" (TestList
    [ testSuccess "simple" () $ parseString punit "a"
    , testSuccess "across lines" "a\nb" $ parseString (pstr "a\nb") "a\nb\nc"
    , testSuccess "empty" () $ parseString punit ""
    , testSuccess "empty line" '\n' $ parseString (pchar '\n') "\n"
    ])

pcharTest :: Test
pcharTest = TestLabel "pchar" (TestList
    [ testSuccess "match" 'a' $ parse (pchar 'a') ["abc"]
    , testFailure "not match" $ parse (pchar 'b') ["abc"]
    , testFailure "empty" $ parse (pchar 'a') []
    ])

pcharInTest :: Test
pcharInTest = TestLabel "pcharIn" (TestList
    [ testSuccess "match" 'c' $ parse (pcharIn "abc") ["c"]
    , testFailure "not match" $ parse (pcharIn "abc") ["d"]
    ])

pcharNotInTest :: Test
pcharNotInTest = TestLabel "pcharNotIn" (TestList
    [ testSuccess "not match" 'd' $ parse (pcharNotIn "abc") ["d"]
    , testFailure "match" $ parse (pcharNotIn "abc") ["c"]
    ])

pdigitTest :: Test
pdigitTest = TestLabel "pdigit" (TestList
    [ TestList (testDigit <$> "0123456789")
    , testFailure "not match" $ parse pdigit ["a"]
    ])
  where
    testDigit :: Char -> Test
    testDigit d = testSuccess "match" d $ parse pdigit [[d]]

pstrTest :: Test
pstrTest = TestLabel "pstr" (TestList
    [ testSuccess "match" "abc" $ parse (pstr "abc") ["abc"]
    , testFailure "no match" $ parse (pstr "bc") ["abc"]
    , testFailure "part match" $ parse (pstr "acb") ["abc"]
    ])

pquotedstrTest :: Test
pquotedstrTest = TestLabel "pquotedstring" (TestList
    [ testSuccess "success" "value" $ parse pquotedstr ["\"value\""]
    , testFailure "no open quote" $ parse pquotedstr ["value\""]
    , testFailure "no close quote" $ parse pquotedstr ["\"value"]
    ])

withErrorTest :: Test
withErrorTest = TestLabel "withError" (TestList
    [ testSuccess "success unchanged" 'a' $ parse (pchar 'a' <?> "my error") ["abc"]
    , TestCase $ assertEqual "failure <?> msg" (Left "my error") $ first message (parse (pchar 'a' <?> "my error") [""])
    , TestCase $ assertEqual "withError msg failure" (Left "my error") $ first message (parse (withError "my error" $ pchar 'a') [""])
    ])

altTest :: Test
altTest = TestLabel "<|>" (TestList
    [ testSuccess "Right <|> Left" 'a' $ parse (pchar 'a' <|> pchar 'b') ["abc"]
    , testSuccess "Left <|> Right" 'b' $ parse (pchar 'a' <|> pchar 'b') ["bc"]
    , testFailure "Left <|> Left" $ parse (pchar 'a' <|> pchar 'b') ["c"]
    ])

combinerTest :: Test
combinerTest = TestLabel "combiners" (TestList [tuples, ignores])
  where
    tuples :: Test
    tuples = TestList
        [ testSuccess "Right .&&. Right" ('a','b') $ parse (pchar 'a' .&&. pchar 'b') ["abc"]
        , testFailure "Left .&&. Right" $ parse (pchar 'a' .&&. pchar 'b') ["bab"]
        , testFailure "Right .&&. Left" $ parse (pchar 'a' .&&. pchar 'b') ["acb"]
        ]
    ignores :: Test
    ignores = TestList
        [ testSuccess "a &&. b" 'b' $ parse (pchar 'a' &&. pchar 'b') ["abc"]
        , testSuccess "a .&& b" 'a' $ parse (pchar 'a' .&& pchar 'b') ["abc"]
        , testFailure "! &&. b" $ parse (pchar 'b' &&. pchar 'a') ["abc"]
        , testFailure "a .&& !" $ parse (pchar 'a' .&& pchar 'c') ["abc"]
        ]

pfilterTest :: Test
pfilterTest = TestLabel "pfilter" (TestList
    [ testSuccess "true" 'a' $ parseString (pfilter (== 'a') $ pchar 'a') "abc"
    , testSuccess "false or true" 'a' $ parseString (pfilter (== 'b') (pchar 'a') <|> pchar 'a') "abc" 
    , testFailure "pfilter false" $ parseString (pfilter (== 'b') $ pchar 'a') "abc"
    , testFailure "pfilter fail" $ parseString (pfilter (== 'a') $ pchar 'b') "abc"
    ])

optionTest :: Test
optionTest = TestLabel "option" (TestList
    [ testSuccess "match" (Just 'a') $ parseString (option $ pchar 'a') "abc"
    , testSuccess "no match" Nothing $ parseString (option $ pchar 'b') "abc"
    ])

oneOrMoreTest :: Test
oneOrMoreTest = TestLabel "oneOrMore" (TestList
    [ testFailure "empty" $ parseString (oneOrMore $ pchar 'a') ""
    , testFailure "none" $ parseString (oneOrMore $ pchar 'b') "abc"
    , testSuccess "one" "a" $ parseString (oneOrMore $ pchar 'a') "abc"
    , testSuccess "more" "aaa" $ parseString (oneOrMore $ pchar 'a') "aaabc"
    ])

zeroOrMoreTest :: Test
zeroOrMoreTest = TestLabel "zeroOrMore" (TestList
    [ testSuccess "empty" [] $ parseString (zeroOrMore $ pchar 'a') ""
    , testSuccess "none" [] $ parseString (zeroOrMore $ pchar 'b') "abc"
    , testSuccess "one" "a" $ parseString (zeroOrMore $ pchar 'a') "abc"
    , testSuccess "more" "aaa" $ parseString (zeroOrMore $ pchar 'a') "aaabc"
    ])

anyOfTest :: Test
anyOfTest = TestLabel "anyOf" (TestList
    [ testSuccess "match" 'a' $ parseString (anyOf [pchar 'c', pchar 'b', pchar 'a']) "abc"
    , testFailure "no match" $ parseString (anyOf [pchar 'b', pchar 'c']) "abc"
    , testFailure "empty parsers" $ parseString (anyOf []) "abc"
    ])

whitespaceTest :: Test
whitespaceTest = TestLabel "whitespace" (TestList
    [ testSuccess "space" ' ' $ parseString whitespace " "
    , testSuccess "tab" '\t' $ parseString whitespace "\t"
    , testSuccess "carriage return" '\r' $ parseString whitespace "\r"
    , testSuccess "new line" '\n' $ parseString whitespace "\n"
    , testSuccess "new lines" '\n' $ parse whitespace [""]
    , testSuccess "multiple new lines" ('\n','\n') $ parse (whitespace .&&. whitespace) ["", ""]
    , testFailure "empty" $ parse whitespace []
    , testFailure "non whitespace" $ parse whitespace ["a"]
    ])

eofTest :: Test
eofTest = TestLabel "eof" (TestList
    [ testSuccess "empty string" () $ parseString eof ""
    , testSuccess "empty source" () $ parse eof []
    , testFailure "non-empty" $ parse eof [""]
    ])
