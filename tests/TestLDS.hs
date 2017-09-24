module TestLDS where

import Test.HUnit
import LDS
import FunctionStore
import TestFixtures

---------------
-- TESTCASE #0
---------------
main :: IO Counts
main = runTestTT $ TestList [
        ]
