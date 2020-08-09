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
    , integrationTest
    ])

evalP :: Parser a -> [String] -> Either String a
evalP p = bimap message fst . parse p

evalPS :: Parser a -> String -> Either String a
evalPS p = bimap message fst . parseString p

xelement :: String -> String -> XElement
xelement n v = XElement (XName n) (Value v)

pelementTest :: Test
pelementTest = TestLabel "pelement" (TestList
    [ TestCase $ assertEqual "standard tags" (Right $ xelement "name" "value") $ evalPS pelement "<name>value</name>"
    , TestCase $ assertEqual "inline tag" (Right $ xelement "name" "") $ evalPS pelement "<name />"
    , TestCase $ assertEqual "empty tag" (Right $ xelement "name" "") $ evalPS pelement "<name></name>"
    , TestCase $ assertBool "no end tag" $ isLeft (evalPS pelement "<name>value</othername>")
    , TestCase $ assertBool "no end inline" $ isLeft (evalPS pelement "<name /")
    , TestCase $ assertBool "no slash inline" $ isLeft (evalPS pelement "<name >")
    , TestCase $ assertBool "empty tag" $ isLeft (evalPS pelement "<>value</>")
    ])

integrationTest :: Test
integrationTest = TestCase $ assertEqual "integration" (Right expected) $ evalP xmlParser xml
  where
    xml =
        [ "<root>"
        , "   <items>"
        , "       <item>"
        , "           value"
        , "       </item>"
        , "   </items>"
        , "</root>"
        ]
    expected = XDocument
        ( XElement (XName "root")
            ( Element $ XElement (XName "items")
                ( Element $ XElement (XName "item") (Value "value") )
            )
        )
