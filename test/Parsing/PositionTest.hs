module Parsing.PositionTest where

import Test.HUnit

import Parsing.Position

suite :: Test
suite = TestLabel "Position" (TestList
    [ nextLineTest
    , nextColTest
    ])

nextLineTest :: Test
nextLineTest = TestLabel "nextLine" (TestList
    [ TestCase $ assertEqual "from 0 col" (Position 1 0) $ nextLine new
    , TestCase $ assertEqual "from other col" (Position 1 0) $ nextLine (Position 0 10)
    ])

nextColTest :: Test
nextColTest = TestCase $ assertEqual "nextCol" (Position 0 1) $ nextCol new 
