module Parsing.StringParserCheck where

import Data.Char (isDigit, isSpace, toLower, toUpper)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)

import Test.HUnit
import Test.QuickCheck

import Parsing.StringParser

runSuite :: IO ()
runSuite =
    quickCheck (mkLabel "pwhitespace" prop_pwhitespace)
    *> quickCheck (mkLabel "pchar" prop_pchar)
    *> quickCheck (mkLabel "pchari" prop_pchari)
    *> quickCheck (mkLabel "pstr" prop_pstr)
    *> quickCheck (mkLabel "pstri" prop_pstri)
    *> quickCheck (mkLabel "pquotedstr" prop_pquotedstr)
    *> quickCheck (mkLabel "eol" prop_eol)
    *> quickCheck (mkLabel "eof" prop_eof)
    *> quickCheck (mkLabel "pdigit" prop_pdigit)
    *> quickCheck (mkLabel "pint" prop_pint)
  where
    mkLabel s = label $ "StringParser." <> s


prop_pwhitespace :: Char -> Bool
prop_pwhitespace c
    | isSpace c = Right ((), "") == run pwhitespace [c]
    | otherwise = Left MatchFailure == run pwhitespace [c]

prop_pchar :: String -> Char -> Bool
prop_pchar input c
    | input == "" = Left NoInput == run (pchar c) input
    | head input == c = Right (c, tail input) == run (pchar c) input
    | otherwise = Left MatchFailure == run (pchar c) input

prop_pchari :: String -> Char -> Bool
prop_pchari input c
    | input == "" = Left NoInput == run (pchari c) input
    | toLower (head input) == toLower c = Right (head input, tail input) == run (pchari c) input
    | otherwise = Left MatchFailure == run (pchari c) input

prop_pstr :: String -> String -> Bool
prop_pstr input s
    | input == s = Right (s, "") == run (pstr s) input
    | input == "" = Left NoInput == run (pstr s) input
    | s `isPrefixOf` input = Right (s, drop (length s) input) == run (pstr s) input
    | otherwise = Left MatchFailure == run (pstr s) input

prop_pstri :: String -> String -> Bool
prop_pstri input s
    | fmap toLower input == fmap toLower s = Right (s, "") == run (pstri s) input
    | input == "" = Left NoInput == run (pstri s) input
    | (toLower <$> s) `isPrefixOf` (toLower <$> input) = Right (s, drop (length s) input) == run (pstri s) input
    | otherwise = Left MatchFailure == run (pstri s) input

prop_pquotedstr :: String -> Bool -> Bool -> Bool
prop_pquotedstr s b1 b2
    | null s = Left OpeningQuoteExpected == run pquotedstr s
    | '"' `elem` s = prop_pquotedstr (filter (/= '"') s) b1 b2
prop_pquotedstr s False _ = Left OpeningQuoteExpected == run pquotedstr (s <> "\"")
prop_pquotedstr s _ False = Left ClosingQuoteExpected == run pquotedstr ('"':s)
prop_pquotedstr s True True = Right (s, "") == run pquotedstr ("\"" <> s <> "\"")

prop_eol :: String -> Bool
prop_eol s
    | s == "" = Left NoInput == run eol s
    | take 2 s == "\r\n" = Right ((), drop 2 s) == run eol s
    | (head s) `elem` ['\r', '\n'] = Right ((), tail s) == run eol s
    | otherwise = Left MatchFailure == run eol s

prop_eof :: String -> Bool
prop_eof s
    | s == "" = Right ((), "") == run eof s
    | otherwise = Left ExpectedEndOfInput == run eof s

prop_pdigit :: Char -> Bool
prop_pdigit c
    | isDigit c = Right (c, "") == run pdigit [c]
    | otherwise = Left MatchFailure == run pdigit [c]

prop_pint :: String -> Bool
prop_pint s
    | null s = Left NoInput == run pint s
    | otherwise = case numPrefix s of
        "" -> Left MatchFailure == run pint s
        prefix -> Right (read prefix, drop (length prefix) s) == run pint s
  where
    numPrefix (x:xs)
        | x == '-' = numPrefixIter $ '-' : numPrefixIter xs 
        | otherwise = numPrefixIter (x:xs)
    numPrefix _ = ""

    numPrefixIter (x:xs)
        | isDigit x = x : numPrefixIter xs
        | otherwise = ""
    numPrefixIter _ = ""
