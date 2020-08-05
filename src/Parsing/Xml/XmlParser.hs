module Parsing.Xml.XmlParser
    ( XElement(..)
    , XName(..)
    , XValue(..)
    , pelement
    , pvalue
    ) where

import Parsing.Parser


type XValue = String

newtype XName = XName String deriving (Eq, Show)
data XElement = XElement XName XValue deriving (Eq, Show)


specialChars =
    [ '<'
    , '>'
    ]

pname :: Parser XName
pname = XName <$> oneOrMore (anyOf $ pchar <$> nameChars)
  where
    nameChars = concat
        [ [ 'A'..'Z']
        , [ 'a'..'z']
        , [ '0'..'9']
        ]

openingTag :: Parser XName
openingTag = (pchar '<') &&. pname .&& (pchar '>')

closingTag :: XName -> Parser XName
closingTag n = (pstr "</") &&. (pfilter (== n) pname) .&& (pstr ">")

pvalue :: Parser XValue
pvalue = zeroOrMore $ pfilter (not . flip elem specialChars) panychar

pelement :: Parser XElement
pelement = pinlineElement <|> ptaggedElement

ptaggedElement :: Parser XElement
ptaggedElement = XElement <$> openingTag <*> pvalue >>= \(XElement n v) -> pure (XElement n v) .&& closingTag n

pinlineElement :: Parser XElement
pinlineElement = XElement <$> (pstr "<") &&. pname .&& (pstr " />") <*> noValue
  where
    noValue :: Parser XValue
    noValue = pure ""
