import Data.Functor (void)
import Test.HUnit

import qualified Parsing.ParseErrorTest as ParseError
import qualified Parsing.ParserTest as Parser
import qualified Parsing.PositionTest as Position

fullSuite :: Test
fullSuite = TestList
    [ Parser.suite
    , Position.suite
    , ParseError.suite
    ]

main :: IO ()
main = void $ runTestTT fullSuite
