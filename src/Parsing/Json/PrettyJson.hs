module Parsing.Json.PrettyJson
    ( render
    ) where

import Parsing.Json.JValue (JValue(..))
import Parsing.Json.Prettify (Doc, double, series, string, text)

render :: JValue -> Doc

render JNull = text "null"

render (JBool True) = text "true"
render (JBool False) = text "false"

render (JNumber n) = double n

render (JString s) = string s

render (JObject o) = series '{' '}' field o
  where
    field (name,val) = string name
                    <> text ": "
                    <> render val
