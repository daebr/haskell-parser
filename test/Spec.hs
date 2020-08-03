import Data.Functor (void)
import Test.HUnit

import qualified Parsing.ParserTest as Parser

fullSuite :: Test
fullSuite = TestList
    [ Parser.suite
    ]

main :: IO ()
main = void $ runTestTT fullSuite
