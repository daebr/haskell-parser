module Parsing.ParserTest where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Test.HUnit

import Parsing.Parser

suite :: Test
suite = TestLabel "Parser" $ TestList
    [ anyOfTest
    , anyOfNETest
    , applicativeTest
    , bifunctorTest
    , evalTest
    , execTest
    , failureTest
    , functorTest
    , mapErrorTest
    , monadTest
    , oneOrMoreTest
    , optionTest
    , orElseTest
    , parseTest
    , pendTest
    , setErrorTest
    , runTest
    , zeroOrMoreTest
    ]

punit :: Parser [Int] e ()
punit = pure ()

pequal :: Eq a => a -> Parser [a] MatchError a
pequal = match . (==) 

pnext :: Parser [Int] MatchError Int
pnext = match (const True)

failInt :: e -> Parser [Int] e Int
failInt = failure

----

parseTest :: Test
parseTest = TestCase $ assertEqual "parse" (Right 1) $ parse pnext [1..3]

execTest :: Test
execTest = TestCase $ assertEqual "exec" (Right 1) $ exec pnext [1..3]

evalTest :: Test
evalTest = TestCase $ assertEqual "eval" (Right [2,3]) $ eval pnext [1..3]

runTest :: Test
runTest = TestLabel "run" $ TestCase $ assertEqual "run" (Right (1, [2,3])) $ run pnext [1..3]

failureTest :: Test
failureTest = TestCase $ assertEqual "failure" (Left "error") $ parse (failInt "error") []

failIfTest :: Test
failIfTest = TestLabel "failIf" $ TestList
    [ TestCase $ assertEqual "fail" (Left NoInput) $ parse (failIf (const NoInput) (== 1) pnext) [1..3]
    , TestCase $ assertEqual "succeed" (Right 1) $ parse (failIf (const NoInput) (== 0) pnext) [1..3]
    ]

mapErrorTest :: Test
mapErrorTest = TestLabel "mapError" $ TestList
    [ TestCase $ assertEqual "success" (Right 1) $ parse (mapError id pnext) [1..3]
    , TestCase $ assertEqual "failure" (Left $ show NoInput) $ parse (mapError show pnext)  []
    , TestCase $ assertEqual "infix" (Left $ show NoInput) $ parse (pnext <?> show) []
    ]

setErrorTest :: Test
setErrorTest = TestLabel "setError" $ TestList
    [ TestCase $ assertEqual "success" (Right 1) $ parse (setError "" pnext) [1..3]
    , TestCase $ assertEqual "failure" (Left "a") $ parse (setError "a" pnext) []
    ]

functorTest :: Test
functorTest = TestLabel "functor" $ TestList
    [ TestCase $ assertEqual "success" (Right "1") $ parse (show <$> pnext) [1]
    , TestCase $ assertEqual "failure" (Left 0) $ parse (show <$> failInt 0) []
    ]

bifunctorTest :: Test
bifunctorTest = TestLabel "bifunctor" $ TestList
    [ TestCase $ assertEqual "success" (Right "1") $ parse (bimap id show pnext) [1]
    , TestCase $ assertEqual "failure" (Left "0") $ parse (bimap show id $ failInt 0) []
    ]

applicativeTest :: Test
applicativeTest = TestLabel "applicative" $ TestList
    [ TestCase $ assertEqual "pure" (Right (0, [1])) $ run (pureInt 0) [1]
    , TestCase $ assertEqual "apply" (Right (3, [3])) $ run ((+) <$> pnext <*> pnext) [1..3]
    ]
  where
    pureInt :: Int -> Parser [Int] String Int
    pureInt = pure

monadTest :: Test
monadTest = TestCase $ assertEqual "monad" (Right (3, [3])) $ run (pnext >>= \i -> (+i) <$> pnext) [1..3]

pendTest :: Test
pendTest = TestLabel "pend" $ TestList
    [ TestCase $ assertEqual "end" (Right ()) $ parse pend []
    , TestCase $ assertEqual "not end" (Left ExpectedEndOfInput) $ parse pend [1]
    ]

orElseTest :: Test
orElseTest = TestLabel "orElse" $ TestList
    [ TestCase $ assertEqual "first" (Right 1) $ parse (pnext <||> pequal 0) [1]
    , TestCase $ assertEqual "second" (Right 1) $ parse (pequal 0 <||> pnext) [1]
    , TestCase $ assertEqual "both" (Right 1) $ parse (pequal 1 <||> ((+1) <$> pnext)) [1]
    , TestCase $ assertEqual "neither" (Left "error 1") $ parse (failInt "error 1" <||> failInt "error 2") []
    ]

anyOfTest :: Test
anyOfTest = TestLabel "anyOf" $ TestList
    [ TestCase $ assertEqual "default" (Left "error") $ parse (anyOfInt "error" []) []
    , TestCase $ assertEqual "first" (Right ()) $ parse (anyOf "error" [punit, failure "another error"]) [1]
    , TestCase $ assertEqual "second" (Right ()) $ parse (anyOf "error" [failure "another error", punit]) [1]
    , TestCase $ assertEqual "all" (Right 1) $ parse (anyOf "error" [pequal 1, (+1) <$> pnext]) [1]
    , TestCase $ assertEqual "none" (Left "error 1") $ parse (anyOf "error 1" [failInt "error 2", failInt "error 3"]) []
    ]
  where
    anyOfInt :: e -> [Parser [Int] e Int] -> Parser [Int] e Int
    anyOfInt = anyOf

anyOfNETest :: Test
anyOfNETest = TestLabel "anyOfNE" $ TestList
    [ TestCase $ assertEqual "first" (Right ()) $ parse (anyOfNE $ punit :| [failure "error"]) [1]
    , TestCase $ assertEqual "second" (Right ()) $ parse (anyOfNE $ failure "error" :| [punit]) [1]
    , TestCase $ assertEqual "none" (Left "error 1") $ parse (anyOfNE $ failInt "error 1" :| [failInt "error 2"]) [1]
    ]

optionTest :: Test
optionTest = TestLabel "option" $ TestList
    [ TestCase $ assertEqual "match" (Right $ Just 1) $ parse (option pnext) [1]
    , TestCase $ assertEqual "no match" (Right Nothing) $ parse (option pnext) []
    ]

oneOrMoreTest :: Test
oneOrMoreTest = TestLabel "oneOrMore" $ TestList
    [ TestCase $ assertEqual "none" (Left MatchFailure) $ parse (oneOrMore $ pequal 0) [1]
    , TestCase $ assertEqual "one" (Right [1]) $ parse (oneOrMore pnext) [1]
    , TestCase $ assertEqual "more" (Right [1,2,3]) $ parse (oneOrMore pnext) [1..3]
    ]

zeroOrMoreTest :: Test
zeroOrMoreTest = TestLabel "zeroOrMore" $ TestList
    [ TestCase $ assertEqual "none" (Right []) $ parse (zeroOrMore $ pequal 0) [1]
    , TestCase $ assertEqual "one" (Right [1]) $ parse (zeroOrMore pnext) [1]
    , TestCase $ assertEqual "more" (Right [1,2,3]) $ parse (zeroOrMore pnext) [1..3]
    ]
