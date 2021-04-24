module Parsing.Json.JValue
    ( JValue(..)
    , array
    , boolean
    , double
    , int
    , isNull
    , object
    , render
    , string
    ) where

import Data.List (intercalate)

data JValue
    = JString String
    | JNumber Double
    | JBool Bool
    | JObject [(String, JValue)]
    | JArray [JValue]
    | JNull
  deriving (Eq, Show)

string :: JValue -> Maybe String
string (JString s) = Just s
string _           = Nothing

int :: JValue -> Maybe Int
int = fmap truncate . double

double :: JValue -> Maybe Double
double (JNumber d) = Just d
double _           = Nothing

boolean :: JValue -> Maybe Bool
boolean (JBool b) = Just b
boolean _         = Nothing

object :: JValue -> Maybe [(String, JValue)]
object (JObject o) = Just o
object _           = Nothing

array :: JValue -> Maybe [JValue]
array (JArray a) = Just a
array _          = Nothing

isNull :: JValue -> Bool
isNull JNull = True
isNull _     = False

render :: JValue -> String
render JNull = "null"
render (JBool b) = show b
render (JNumber n) = show n
render (JString s) = s

render (JArray a) = "[" ++ values a ++ "]"
  where
    values = intercalate ", " . map render 

render (JObject o) = "{" ++ pairs o ++ "}"
  where
    pairs [] = ""
    pairs vs = intercalate ", " (map renderPair vs)
    
    renderPair (k,v) = k ++ ": " ++ render v
