import Data.Functor (void)
import Test.HUnit

import qualified Parsing.ParseErrorTest as ParseError
import qualified Parsing.ParserTest as Parser
import qualified Parsing.PositionTest as Position
import qualified Parsing.Xml.XmlParserTest as XmlParser

fullSuite :: Test
fullSuite = TestList
    [ Parser.suite
    , Position.suite
    , ParseError.suite
    , XmlParser.suite
    ]

main :: IO ()
main = void $ runTestTT fullSuite
