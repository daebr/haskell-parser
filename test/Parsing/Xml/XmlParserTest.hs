module Parsing.Xml.XmlParserTest where

import Data.Bifunctor (bimap)
import Data.Either (isLeft)
import Test.HUnit

import Parsing.ParseError (message)
import Parsing.Parser
import Parsing.Xml.XmlParser

suite :: Test
suite = TestLabel "XmlParser" (TestList
    [ pelementTest
    ])

evalP :: Parser a -> String -> Either String a
evalP p = bimap message fst . parseString p

xelement :: String -> String -> XElement
xelement n v = XElement (XName n) v

pelementTest :: Test
pelementTest = TestLabel "pelement" (TestList
    [ TestCase $ assertEqual "standard tags" (Right $ xelement "name" "value") $ evalP pelement "<name>value</name>"
    , TestCase $ assertEqual "inline tag" (Right $ xelement "name" "") $ evalP pelement "<name />"
    , TestCase $ assertEqual "empty tag" (Right $ xelement "name" "") $ evalP pelement "<name></name>"
    , TestCase $ assertBool "no end tag" $ isLeft (evalP pelement "<name>value</othername>")
    , TestCase $ assertBool "no end inline" $ isLeft (evalP pelement "<name /")
    , TestCase $ assertBool "no slash inline" $ isLeft (evalP pelement "<name >")
    , TestCase $ assertBool "empty tag" $ isLeft (evalP pelement "<>value</>")
    ])
