module Parsing.Xml.XmlParser
    ( XDocument(..)
    , XNode(..)
    , XElement(..)
    , XName(..)
    , XValue(..)
    , xmlParser
    , pnode
    , pelement
    , pvalue
    ) where

import Data.Function ((&))
import Data.List.Extra (trim)

import Parsing.Parser


type XValue = String
newtype XName = XName String deriving (Eq, Show)
data XElement = XElement XName XNode deriving (Eq, Show)
data XNode
    = Element XElement
    | Value XValue
  deriving (Eq, Show)
newtype XDocument = XDocument { root :: XElement } deriving (Eq, Show)

xmlParser :: Parser XDocument
xmlParser = XDocument <$> pelement

pname :: Parser XName
pname = XName <$> oneOrMore (anyOf $ pchar <$> nameChars)
        & withError "Invalid name"
  where
    nameChars = concat
        [ [ 'A'..'Z']
        , [ 'a'..'z']
        , [ '0'..'9']
        ]

anyWhitespace :: Parser String
anyWhitespace = zeroOrMore whitespace

openingTag :: Parser XName
openingTag = (pchar '<') &&. pname .&& (pchar '>')

closingTag :: XName -> Parser XName
closingTag n = (pstr "</") &&. (pfilter (== n) pname) .&& (pstr ">")

pvalue :: Parser XValue
pvalue = fmap trim . zeroOrMore $ pfilter (not . flip elem disallowedChars) panychar
  where
    disallowedChars = [ '<', '>', '\r', '\n' ]

pelement :: Parser XElement
pelement = pinlineElement <|> ptaggedElement

ptaggedElement :: Parser XElement
ptaggedElement = XElement 
                 <$> openingTag
                 <*> anyWhitespace &&. pnode .&& anyWhitespace
                 >>= \(XElement n v) -> pure (XElement n v) .&& anyWhitespace .&& closingTag n .&& anyWhitespace

pinlineElement :: Parser XElement
pinlineElement = XElement <$> (pstr "<") &&. pname .&& (pstr " />") <*> noValue
  where
    noValue :: Parser XNode
    noValue = pure $ Value ""

pnode :: Parser XNode
pnode = anyWhitespace &&. (Element <$> pelement <|> Value <$> pvalue) .&& anyWhitespace
        & withError "Invalid node"
