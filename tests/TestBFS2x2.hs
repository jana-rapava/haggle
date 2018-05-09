module TestBFS2x2 where

import Test.HUnit
import BFS
import InfInt
import TestFixtures
import Success2x2
import Lloyd15

---------------
-- TESTCASE #0
---------------
testSearchBFS0_11 :: Test
testSearchBFS0_11 = TestCase $ assertEqual ""
        [[board0, board0c]]
        (testBfs board0c 11)

testSearchBFS0_12 :: Test
testSearchBFS0_12 = TestCase $ assertEqual ""
        paths0rev
        (testBfs board0c 12)

testSearchBFS0_13 :: Test
testSearchBFS0_13 = TestCase $ assertEqual ""
        paths0rev
        (testBfs board0c 13)

testSearchBFS0 :: Test
testSearchBFS0 = TestCase $ assertEqual ""
        paths0rev
        (testBfs board0c Inf)

testSearchBFS0Fail_5 :: Test
testSearchBFS0Fail_5 = TestCase $ assertEqual ""
        []
        (testBfs board0n 5)

testSearchBFS0Fail_6 :: Test
testSearchBFS0Fail_6 = TestCase $ assertEqual ""
        []
        (testBfs board0n 6)

testSearchBFS0Fail :: Test
testSearchBFS0Fail = TestCase $ assertEqual ""
        []
        (testBfs board0n Inf)

main :: IO Counts
main = runTestTT $ TestList [
        testSearchBFS0_11,
        testSearchBFS0_12,
        testSearchBFS0_13,
        testSearchBFS0,
        testSearchBFS0Fail_5,
        testSearchBFS0Fail_6,
        testSearchBFS0Fail
        ]
