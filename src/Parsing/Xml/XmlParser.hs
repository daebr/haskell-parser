{-# LANGUAGE FlexibleInstances #-}

module Parsing.Xml.XmlParser
    ( XDocument(..)
    , XNode(..)
    , XElement(..)
    , XAttr(..)
    , XName(..)
    , XValue(..)
    , xmlParser
    , pnode
    , pelement
    , pvalue
    ) where

import Data.Function ((&))
import Data.List (intersperse)
import Data.List.Extra (trim)

import Parsing.Parser

class Xml a where
    toXml :: a -> String

type XValue = String

newtype XName = XName String deriving (Eq, Show)

data XAttr = XAttr XName XValue deriving (Eq, Show)

data XElement = XElement XName [XAttr] XNode deriving (Eq, Show)

data XNode
    = Elements [XElement]
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

pvalue :: Parser XValue
pvalue = fmap trim . zeroOrMore $ pfilter (not . flip elem disallowedChars) panychar
  where
    disallowedChars = [ '<', '>', '\r', '\n' ]

pelement :: Parser XElement
pelement = ptaggedElement

ptaggedElement :: Parser XElement
ptaggedElement = XElement 
                 <$> (pchar '<') &&. pname .&& (pchar '>')
                 <*> pure [] 
                 <*> anyWhitespace &&. pnode .&& anyWhitespace
                 >>= \(XElement name attrs node) -> pure (XElement name attrs node) .&& anyWhitespace .&& closingTag name .&& anyWhitespace
  where
    closingTag :: XName -> Parser XName
    closingTag n = (pstr "</") &&. (pfilter (== n) pname) .&& (pstr ">")
                   -- & withError "Closing tag expected for '" <> toXml n <> "'"

pinlineElement :: Parser XElement
pinlineElement = XElement <$> (pstr "<") &&. pname .&& (pstr " />") <*> pure [] <*> noValue
  where
    noValue :: Parser XNode
    noValue = pure $ Value ""

pnode :: Parser XNode
pnode = Elements <$> pelementList <|> Value <$> pvalue
        & withError "Invalid node"
  where
    pelementList :: Parser [XElement]
    pelementList = zeroOrMore (pelement .&& anyWhitespace)

instance Xml XValue where
    toXml s = s

instance Xml XName where
    toXml (XName n) = n

instance Xml XAttr where
    toXml (XAttr n v) = toXml n <> "=\"" <> v <> "\""

instance Xml XElement where
    toXml (XElement n attrs node) = case node of
        Value "" -> "<" <> toXml n <> " " <> attrString <> " />"
        otherwise ->
            unlines [ "<" <> toXml n <> " " <> attrString <> ">"
                    , toXml node
                    , "</" <> toXml n <> ">"
                    ]
      where
        attrString = mconcat . intersperse " " $ toXml <$> attrs

instance Xml XNode where
    toXml (Elements es) = unlines $ toXml <$> es
    toXml (Value v) = v

instance Xml XDocument where
    toXml = toXml . root
