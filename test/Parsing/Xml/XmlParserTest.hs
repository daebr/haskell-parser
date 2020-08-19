module Parsing.Xml.XmlParserTest where

import Data.Bifunctor (second)
import Data.Either (isLeft)
import Test.HUnit

import Parsing.ParseError (ParseError)
import Parsing.Parser
import Parsing.Xml.XmlParser

suite :: Test
suite = TestLabel "XmlParser" (TestList
    [ pelementTest
    , integrationTest
    ])

evalP :: Parser a -> [String] -> Either ParseError a
evalP p = second fst . parse p

evalPS :: Parser a -> String -> Either ParseError a
evalPS p = second fst . parseString p

xelement :: String -> [XAttr] -> String -> XElement
xelement n as [] = XElement (XName n) as Empty
xelement n as v = XElement (XName n) as (Value v)

xattr :: String -> String -> XAttr
xattr n = XAttr (XName n)

pelementTest :: Test
pelementTest = TestLabel "pelement" (TestList
    [ TestCase $ assertEqual "standard tags" (Right $ xelement "name" [] "value") $ evalPS pelement "<name>value</name>"
    , TestCase $ assertEqual "inline tag" (Right $ xelement "name" [] "") $ evalPS pelement "<name />"
    , TestCase $ assertEqual "empty tag" (Right $ xelement "name" [] "") $ evalPS pelement "<name></name>"
    , TestCase $ assertEqual "standard w/ attrs" (Right $ xelement "name" [xattr "attr1" "1", xattr "attr2" "2"] "") $ evalPS pelement "<name attr1=\"1\" attr2=\"2\"></name>"
    , TestCase $ assertEqual "inline w/ attrs" (Right $ xelement "name" [xattr "attr1" "1", xattr "attr2" "2"] "") $ evalPS pelement "<name attr1=\"1\" attr2=\"2\" />"
    , TestCase $ assertBool "no end tag" $ isLeft (evalPS pelement "<name>value</othername>")
    , TestCase $ assertBool "no end inline" $ isLeft (evalPS pelement "<name /")
    , TestCase $ assertBool "no slash inline" $ isLeft (evalPS pelement "<name >")
    , TestCase $ assertBool "empty tag" $ isLeft (evalPS pelement "<>value</>")
    , TestCase $ assertBool "no closing attr quote" $ isLeft (evalPS pelement "<name attr=\"1 />")
    , TestCase $ assertBool "no attr quote" $ isLeft (evalPS pelement "<name attr=1 />")
    ])

integrationTest :: Test
integrationTest = TestCase $ assertEqual "integration" (Right expected) $ evalP xmlParser xml
  where
    xml =
        [ "<root>"
        , "   <items>"
        , "       <item id=\"1\">"
        , "           value 1"
        , "       </item>"
        , "       <item id=\"2\">"
        , "           value 2"
        , "       </item>"
        , "       <item id=\"3\" />"
        , "   </items>"
        , "</root>"
        ]
    expected = XDocument
        ( XElement (XName "root") []
            ( Elements 
                [ XElement (XName "items") []
                    ( Elements
                        [ XElement (XName "item") [XAttr (XName "id") "1"] (Value "value 1")
                        , XElement (XName "item") [XAttr (XName "id") "2"] (Value "value 2")
                        , XElement (XName "item") [XAttr (XName "id") "3"] Empty
                        ]
                    )
                ]
            )
        )
