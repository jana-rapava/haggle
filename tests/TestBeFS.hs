module TestBeFS where

import Test.HUnit
import BeFS
import FunctionStore
import TestFixtures

---------------
-- TESTCASE #0
---------------

testPickAndMerge1 :: Test
testPickAndMerge1 = TestCase $ assertEqual ""
        paths1 (pickAndMerge rankBasic nextBoards1 path1)

testPickAndMerge2 :: Test
testPickAndMerge2 = TestCase $ assertEqual ""
        paths2 (pickAndMerge xrank11 nextBoards1 path1)

testAddTo :: Test
testAddTo = TestCase $ assertEqual ""
        paths5 (addTo paths3 paths4)

testSearchFirstBeFS0 :: Test
testSearchFirstBeFS0 = TestCase $ assertEqual ""
        path0
        (head $ befs board0'
--        (fromJust $ searchFirst board0' blank0
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )


testSearchBeFS0Basic :: Test
testSearchBeFS0Basic = TestCase $ assertEqual ""
        paths0
        (befs board0'
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

-- testSearchBeFS0Basic2 :: Test
-- testSearchBeFS0_2 = TestCase $ assertEqual ""
--         paths0rev
--         (dfs board0'
--         (FS {
--         stopSuccess = stopSuccess0,
--         stopFail = stopFail0,
--         rank = rankBasic2,
--         prune = pruneBasic})
--         )

testSearchBeFS0Fail :: Test
testSearchBeFS0Fail = TestCase $ assertEqual ""
        []
        (befs aaboard
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

--------------------
-- TESTCASE #1
--------------------

testSearchFirstBeFS1Basic :: Test
testSearchFirstBeFS1Basic = TestCase $ assertEqual ""
        rightmost1
        (head $ befs xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchFirstBeFS1Xrank11 :: Test
testSearchFirstBeFS1Xrank11 = TestCase $ assertEqual ""
        rightmost1
        (head $ befs xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank11,
        prune = pruneBasic})
        )

testSearchFirstBeFS1Xrank12 :: Test
testSearchFirstBeFS1Xrank12 = TestCase $ assertEqual ""
        rightmost1
        (head $ befs xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

testSearchFirstBeFS1Xxboard1 :: Test
testSearchFirstBeFS1Xxboard1 = TestCase $ assertEqual ""
         [board1, xxboard1]
         (head $ befs xxboard1
         (FS {
         stopSuccess = stopSuccess1,
         stopFail = stopFail1,
         rank = rankBasic,
         prune = pruneBasic})
         )

testSearchBeFS1GetSecond :: Test
testSearchBeFS1GetSecond = TestCase $ assertEqual ""
        [board1, xboard16, xboard15, xboard14, xboard13, xboard12, xboard11, xboard10, xboard9, xboard8, xboard7,xxboard1]
        (head $ drop 1 $ befs xxboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

---------------------
-- TESTCASE #2
---------------------

-- testSearchFirstBeFS2Basic :: Test
-- testSearchFirstBeFS2 = TestCase $ assertEqual ""
--        rightmost2
--        (head $ befs yboard1
--        (FS {
--        stopSuccess = stopSuccess2,
--        stopFail = stopFail2,
--        rank = rankBasic,
--        prune = pruneBasic})
--        )
--
-- testSearchFirstBeFS2Xrank21 :: Test
-- testSearchFirstBeFS6 = TestCase $ assertEqual ""
--        rightmost2
--        (head $ befs yboard1
--        (FS {
--        stopSuccess = stopSuccess2,
--        stopFail = stopFail2,
--        rank = xrank21,
--        prune = pruneBasic})
--        )
--
-- testSearchFirstBeFSXrank22 :: Test
-- testSearchFirstBeFS7 = TestCase $ assertEqual ""
--        rightmost2
--        (head $ befs yboard1
--        (FS {
--        stopSuccess = stopSuccess2,
--        stopFail = stopFail2,
--        rank = xrank22,
--        prune = pruneBasic})
--        )

---------------------------
-- TESTCASE #3
---------------------------

-- testSearchFirstBeFS3 :: Test
-- testSearchFirstBeFS3 = TestCase $ assertEqual ""
--        rightmost3
--        (head $ befs zboard1
--        (FS {
--        stopSuccess = stopSuccess3,
--        stopFail = stopFail3,
--        rank = rankBasic,
--        prune = pruneBasic})
--        )
--
-- testSearchFirstBeFS8 :: Test
-- testSearchFirstBeFS8 = TestCase $ assertEqual ""
--        rightmost3
--        (head $ befs zboard1
--        (FS {
--        stopSuccess = stopSuccess3,
--        stopFail = stopFail3,
--        rank = xrank31,
--        prune = pruneBasic})
--        )
--
-- testSearchFirstBeFS9 :: Test
-- testSearchFirstBeFS9 = TestCase $ assertEqual ""
--        rightmost3
--        (head $ befs zboard1
--        (FS {
--        stopSuccess = stopSuccess3,
--        stopFail = stopFail3,
--        rank = xrank32,
--        prune = pruneBasic})
--        )
main :: IO Counts
main = runTestTT $ TestList [
        testPickAndMerge1,
        testPickAndMerge2,
        testAddTo,
        testSearchFirstBeFS0,
        testSearchBeFS0Basic,
        testSearchBeFS0Fail,
        --testSearchFirstBeFS1Basic,
        testSearchFirstBeFS1Xrank11,
        testSearchFirstBeFS1Xrank12,
        --testSearchFirstBeFS1Xxboard1,
        testSearchBeFS1GetSecond
        ]
