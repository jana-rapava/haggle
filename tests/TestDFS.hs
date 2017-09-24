module TestDFS where

import Test.HUnit
import DFS
import FunctionStore
import TestFixtures

---------------
-- TESTCASE #0
---------------

testSearchFirstDFS0 :: Test
testSearchFirstDFS0 = TestCase $ assertEqual ""
        [board0, board0']
        (head $ dfs board0'
--        (fromJust $ searchFirst board0' blank0
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchDFS0Basic :: Test
testSearchDFS0Basic = TestCase $ assertEqual ""
        paths0
        (dfs board0'
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

-- testSearchDFS0Basic2 :: Test
-- testSearchDFS0_2 = TestCase $ assertEqual ""
--         paths0rev
--         (dfs board0'
--         (FS {
--         stopSuccess = stopSuccess0,
--         stopFail = stopFail0,
--         rank = rankBasic2,
--         prune = pruneBasic})
--         )


testSearchDFS0Fail :: Test
testSearchDFS0Fail = TestCase $ assertEqual ""
        []
        (dfs aaboard
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

------------------
-- TESTCASE #1
------------------

testSearchFirstDFS1Basic :: Test
testSearchFirstDFS1Basic = TestCase $ assertEqual ""
        rightmost1
        (head $ dfs xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchFirstDFS1Xrank11 :: Test
testSearchFirstDFS1Xrank11 = TestCase $ assertEqual ""
        rightmost1
        (head $ dfs xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank11,
        prune = pruneBasic})
        )


testSearchFirstDFS1Xrank12 :: Test
testSearchFirstDFS1Xrank12 = TestCase $ assertEqual ""
        rightmost1
        (head $ dfs xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )


testSearchFirstDFS1Xxboard1 :: Test
testSearchFirstDFS1Xxboard1 = TestCase $ assertEqual ""
         [board1, xxboard1]
         (head $ dfs xxboard1
         (FS {
         stopSuccess = stopSuccess1,
         stopFail = stopFail1,
         rank = rankBasic,
         prune = pruneBasic})
         )

testSearchDFS1GetSecond :: Test
testSearchDFS1GetSecond = TestCase $ assertEqual ""
        [board1, xboard16, xboard15, xboard14, xboard13, xboard12, xboard11, xboard10, xboard9, xboard8, xboard7,xxboard1]
        (head $ drop 1 $ dfs xxboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

------------------
-- TESTCASE #2
-----------------

-- testSearchFirstDFS2 :: Test
-- testSearchFirstDFS2 = TestCase $ assertEqual ""
--        rightmost2
--        (head $ dfs yboard1
--        (FS {
--        stopSuccess = stopSuccess2,
--        stopFail = stopFail2,
--        rank = rankBasic,
--        prune = pruneBasic})
--        )
--
--
-- testSearchFirstDFS6 :: Test
-- testSearchFirstDFS6 = TestCase $ assertEqual ""
--        rightmost2
--        (head $ dfs yboard1
--        (FS {
--        stopSuccess = stopSuccess2,
--        stopFail = stopFail2,
--        rank = xrank21,
--        prune = pruneBasic})
--        )
--
--
-- testSearchFirstDFS7 :: Test
-- testSearchFirstDFS7 = TestCase $ assertEqual ""
--        rightmost2
--        (head $ dfs yboard1
--        (FS {
--        stopSuccess = stopSuccess2,
--        stopFail = stopFail2,
--        rank = xrank22,
--        prune = pruneBasic})
--        )

--------------------
-- TESTCASE #3
--------------------

-- testSearchFirstDFS3 :: Test
-- testSearchFirstDFS3 = TestCase $ assertEqual ""
--        rightmost3
--        (head $ dfs zboard1
--        (FS {
--        stopSuccess = stopSuccess3,
--        stopFail = stopFail3,
--        rank = rankBasic,
--        prune = pruneBasic})
--        )
--
--
-- testSearchFirstDFS8 :: Test
-- testSearchFirstDFS8 = TestCase $ assertEqual ""
--        rightmost3
--        (head $ dfs zboard1
--        (FS {
--        stopSuccess = stopSuccess3,
--        stopFail = stopFail3,
--        rank = xrank31,
--        prune = pruneBasic})
--        )
--
--
-- testSearchFirstDFS9 :: Test
-- testSearchFirstDFS9 = TestCase $ assertEqual ""
--        rightmost3
--        (head $ dfs zboard1
--        (FS {
--        stopSuccess = stopSuccess3,
--        stopFail = stopFail3,
--        rank = xrank32,
--        prune = pruneBasic})
--        )
main :: IO Counts
main = runTestTT $ TestList [
        testSearchFirstDFS0,
        testSearchDFS0Basic,
        testSearchDFS0Fail,
        testSearchFirstDFS1Basic,
        testSearchFirstDFS1Xrank11,
        testSearchFirstDFS1Xrank12,
        testSearchFirstDFS1Xxboard1,
        testSearchDFS1GetSecond
        ]
