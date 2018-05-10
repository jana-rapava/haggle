module TestBeFS2x2 where

import Test.HUnit
import BeFS
import TestFixtures
import Lloyd15
import Success2x2

---------------
-- TESTCASE #0
---------------

testSearchFirstBeFS0c :: Test
testSearchFirstBeFS0c = TestCase $ assertEqual ""
        path0
        (head $ testBefs board0c rankBasic)

testSearchBeFS0c :: Test
testSearchBeFS0c = TestCase $ assertEqual ""
        paths0
        (testBefs board0c rankBasic)

testSearchBeFS0n :: Test
testSearchBeFS0n = TestCase $ assertEqual ""
        []
        (testBefs board0n rankBasic)

main :: IO Counts
main = runTestTT $ TestList [
        testSearchFirstBeFS0c,
        testSearchBeFS0c,
        testSearchBeFS0n
        ]
