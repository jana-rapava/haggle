{-# LANGUAGE FlexibleInstances #-}

module TestBFS4x4 where

import Test.HUnit
import BFS
import InfInt
--import FunctionStore
import TestFixtures
import Lloyd15
import Success4x4
import Expandable


---------------
-- TESTCASE #1
---------------
testSearchFirstBFS1y_2 :: Test
testSearchFirstBFS1y_2 = TestCase $ assertEqual ""
         []
         (testBfs board1y 2)

testSearchFirstBFS1y_4 :: Test
testSearchFirstBFS1y_4 = TestCase $ assertEqual ""
         rightmost1
         (head $ testBfs board1y 4)

testSearchFirstBFS1y :: Test
testSearchFirstBFS1y = TestCase $ assertEqual ""
        rightmost1
        (head $ testBfs board1y Inf)

testSearchFirstBFS1x_1 :: Test
testSearchFirstBFS1x_1 = TestCase $ assertEqual ""
        []
        (testBfs board1x 1)

testSearchFirstBFS1x_2 :: Test
testSearchFirstBFS1x_2 = TestCase $ assertEqual ""
        [board1, board1x]
        (head $ testBfs board1x 2)

testSearchFirstBFS1x :: Test
testSearchFirstBFS1x = TestCase $ assertEqual ""
        [board1, board1x]
        (head $ testBfs board1x Inf)

main :: IO Counts
main = runTestTT $ TestList [
        testSearchFirstBFS1y_2,
        testSearchFirstBFS1y_4,
        testSearchFirstBFS1y,
        testSearchFirstBFS1x_1,
        testSearchFirstBFS1x_2,
        testSearchFirstBFS1x
        ]
