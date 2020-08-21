module Parsing.Xml.XmlParserTest where

import Data.Bifunctor (second)
import Data.Either (isLeft)
import Test.HUnit

import Parsing.ParseError (ParseError)
import Parsing.Parser
import Parsing.Xml.XmlParser

suite :: Test
suite = TestLabel "XmlParser" (TestList
    [ emptyDocumentTest
    , rootTest
    , elementTest
    , attributeTest
    , integrationTest
    ])

eval :: [String] -> Either ParseError XDocument
eval = second fst . parse xmlParser

evalS :: String -> Either ParseError XDocument
evalS = second fst . parseString xmlParser

xelement :: String -> [XAttr] -> String -> XElement
xelement n as [] = XElement (XName n) as Empty
xelement n as v = XElement (XName n) as (Value v)

xattr :: String -> String -> XAttr
xattr n = XAttr (XName n)

emptyDocumentTest :: Test
emptyDocumentTest = TestLabel "empty doc" (TestList
    [ TestCase $ assertBool "empty lines" $ isLeft (eval [])
    , TestCase $ assertBool "empty line string" $ isLeft (eval [""])
    , TestCase $ assertBool "empty string" $ isLeft (evalS "")
    ])

rootTest :: Test
rootTest = TestLabel "root" (TestList
    [ TestCase $ assertEqual "tag" expected $ eval emptyRoot
    , TestCase $ assertEqual "inline" expected $ eval emptyRootInline
    , TestCase $ assertEqual "string" expected $ evalS emptyRootString
    , TestCase $ assertEqual "one line string" expected $ evalS emptyRootStringOneLine
    ])
  where
    expected = Right . XDocument $ XElement (XName "root") [] Empty
    emptyRoot =
        [ "<root>"
        , "</root>"
        ]
    emptyRootInline = [ "<root />" ]
    emptyRootString = "<root>\n</root>"
    emptyRootStringOneLine = "<root></root>"

elementTest :: Test
elementTest = TestList
    [ shallowElementTest
    , nestedElementTest
    ]

shallowElementTest :: Test
shallowElementTest = TestLabel "standalone element" (TestList
    [ success "single" (mkDoc ["element1"]) $ mkXml ["<element1 />"]
    , success "multiple" (mkDoc ["element1", "element2"]) $ mkXml ["<element1 />", "<element2 />"]
    , success "dashed-name" (mkDoc ["element-1"]) $ mkXml ["<element-1 />"]
    , success "one letter name" (mkDoc ["a"]) $ mkXml ["<a />"]
    , success "mix of styles" (mkDoc ["element1", "element2"]) $ mkXml ["<element1>", "</element1>", "<element2 />"]
    , failure "malformed tag" $ mkXml ["<element1", "</element1>"]
    , failure "no closing tag" $ mkXml ["<element1>"]
    , failure "space in name" $ mkXml ["<element 1>", "</element 1>"]
    , failure "wrong inline slash" $ mkXml ["<element1 \\>"]
    , failure "dash-at-start" $ mkXml ["<-element />"]
    , failure "dash-at-end" $ mkXml ["<element- />"]
    ])
  where
    success :: String -> XDocument -> [String] -> Test
    success name expected xml = TestCase $ assertEqual name (Right expected) $ eval xml

    failure :: String -> [String] -> Test
    failure name xml = TestCase $ assertBool name $ isLeft (eval xml)

    mkDoc es = XDocument $ XElement (XName "root") [] $ Elements $ (\n -> xelement n [] "") <$> es
    mkXml es = concat
        [ [ "<root>" ]
        , ("  " <>) <$> es
        , [ "</root>" ]
        ]

nestedElementTest :: Test
nestedElementTest = TestLabel "nested element" (TestList
    [ success "single" (mkDoc ["nested-element"]) $ mkXml ["<nested-element />"]
    , success "multiple" (mkDoc ["nested-element-1", "nested-element-2"]) $ mkXml ["<nested-element-1 />", "<nested-element-2 />"]
    ])
  where
    success :: String -> XDocument -> [String] -> Test
    success name expected xml = TestCase $ assertEqual name (Right expected) $ eval xml

    mkDoc es = 
        XDocument
        $ XElement (XName "root") [] 
            $ Elements $ [ XElement (XName "element") []
                $ Elements $ (\n -> xelement n [] "") <$> es
            ]
    mkXml es = concat
        [ [ "<root>" ]
        , [ "  <element>" ]
        , ("    " <>) <$> es
        , [ "  </element>" ]
        , [ "</root>" ]
        ]


attributeTest :: Test
attributeTest = TestLabel "attribute" (TestList
    [ success "single attribute" [xattr "name" "value"] "name=\"value\""
    , success "multiple attributes" [xattr "name1" "value 1", xattr "name2" "value 2"] "name1=\"value 1\" name2=\"value 2\""
    , success "dashed-name" [xattr "attr-name" "value"] "attr-name=\"value\""
    , failure "no quotes" "name=value"
    , failure "no opening quote" "name=value\""
    , failure "no closing quote" "name=\"value"
    , failure "no equals" "name \"value\""
    , failure "space in name" "name 1=\"value\""
    , failure "spaced equals" "name = \"value\""
    ])
  where
    failure name attrs = TestCase $ assertBool name $ isLeft (mkActualTagged attrs)

    success name expected attrs = TestLabel name (TestList
        [ TestCase $ assertEqual "tagged" (mkExpected expected) (mkActualTagged attrs)
        , TestCase $ assertEqual "inline" (mkExpected expected) (mkActualInline attrs)
        ])

    mkExpected attrs = Right . XDocument $ XElement (XName "root") attrs Empty

    mkActualTagged attrs = evalS $ "<root " <> attrs <> "></root>"
    mkActualInline attrs = evalS $ "<root " <> attrs <> " />" 

integrationTest :: Test
integrationTest = TestCase $ assertEqual "integration" (Right expected) $ eval xml
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
