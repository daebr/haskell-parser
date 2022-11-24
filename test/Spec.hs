import Data.Functor (void)
import Test.HUnit
import Test.QuickCheck

import qualified Parsing.ParserTest as Parser
import qualified Parsing.PositionTest as Position
import qualified Parsing.StringParserCheck as StringParser

main :: IO ()
main = do
    runQuickCheck
    runHUnit

runHUnit :: IO ()
runHUnit = void $ runTestTT suite
  where
    suite = TestList
        [ Parser.suite
        , Position.suite
        ]

runQuickCheck :: IO ()
runQuickCheck = StringParser.runSuite
