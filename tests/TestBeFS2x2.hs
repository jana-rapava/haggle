{-# LANGUAGE FlexibleInstances #-}

module TestBeFS2x2 where

import Test.HUnit
import BeFS
-- import FunctionStore
import TestFixtures
import Lloyd15
import Success2x2
import Expandable

---------------
-- TESTCASE #0
---------------

-- testPickAndMerge1 :: Test
-- testPickAndMerge1 = TestCase $ assertEqual ""
--         paths1 (pickAndMerge rankBasic nextBoards1 path1)
--
-- testPickAndMerge2 :: Test
-- testPickAndMerge2 = TestCase $ assertEqual ""
--         paths2 (pickAndMerge xrank11 nextBoards1 path1)
--
-- testAddTo :: Test
-- testAddTo = TestCase $ assertEqual ""
--         paths5 (addTo paths3 paths4)

testSearchFirstBeFS0c :: Test
testSearchFirstBeFS0c = TestCase $ assertEqual ""
        path0
        (head $ testBefs board0c rankBasic)


testSearchBeFS0c :: Test
testSearchBeFS0c = TestCase $ assertEqual ""
        paths0
        (testBefs board0c rankBasic)

-- testSearchBeFS0Basic2 :: Test
-- testSearchBeFS0_2 = TestCase $ assertEqual ""
--         paths0rev
--         (dfs board0c
--         (FS {
--         stopSuccess = stopSuccess0,
--         stopFail = stopFail0,
--         rank = rankBasic2,
--         prune = pruneBasic})
--         )

testSearchBeFS0n :: Test
testSearchBeFS0n = TestCase $ assertEqual ""
        []
        (testBefs board0n rankBasic)

main :: IO Counts
main = runTestTT $ TestList [
--        testPickAndMerge1,
--        testPickAndMerge2,
--        testAddTo,
        testSearchFirstBeFS0c,
        testSearchBeFS0c,
        testSearchBeFS0n--,
--         testSearchFirstBeFS1Basic,
--         testSearchFirstBeFS1Xrank11,
--         testSearchFirstBeFS1Xrank12,
--         testSearchFirstBeFS1Xxboard1,
--         testSearchBeFS1GetSecond,
--         testSearchFirstBeFS0
        ]
