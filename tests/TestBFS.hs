module TestBFS where

import Test.HUnit
import BFS
import InfInt
import FunctionStore
import TestFixtures

---------------
-- TESTCASE #0
---------------

testSearchBFS0_11 :: Test
testSearchBFS0_11 = TestCase $ assertEqual ""
        [[board0, board0']]
        (bfs board0' 11
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS0_12 :: Test
testSearchBFS0_12 = TestCase $ assertEqual ""
        paths0
        (bfs board0' 12
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS0_13 :: Test
testSearchBFS0_13 = TestCase $ assertEqual ""
        paths0
        (bfs board0' 13
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS0 :: Test
testSearchBFS0 = TestCase $ assertEqual ""
        paths0
        (bfs board0' Inf
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS0Fail_5 :: Test
testSearchBFS0Fail_5 = TestCase $ assertEqual ""
        []
        (bfs aaboard 5
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS0Fail_6 :: Test
testSearchBFS0Fail_6 = TestCase $ assertEqual ""
        []
        (bfs aaboard 6
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS0Fail :: Test
testSearchBFS0Fail = TestCase $ assertEqual ""
        []
        (bfs aaboard Inf
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

----------------------------
-- TESTCASE #1
---------------------------

testSearchFirstBFS1Basic_4 :: Test
testSearchFirstBFS1Basic_4 = TestCase $ assertEqual ""
         rightmost1
         (head $ bfs xboard1 4
         (FS {
         stopSuccess = stopSuccess1,
         stopFail = stopFail1,
         rank = rankBasic,
         prune = pruneBasic})
         )

testSearchFirstBFS1Xrank11_4 :: Test
testSearchFirstBFS1Xrank11_4 = TestCase $ assertEqual ""
         rightmost1
         (head $ bfs xboard1 4
         (FS {
         stopSuccess = stopSuccess1,
         stopFail = stopFail1,
         rank = xrank11,
         prune = pruneBasic})
         )

testSearchFirstBFS1Xrank12_4 :: Test
testSearchFirstBFS1Xrank12_4 = TestCase $ assertEqual ""
         rightmost1
         (head $ bfs xboard1 4
         (FS {
         stopSuccess = stopSuccess1,
         stopFail = stopFail1,
         rank = xrank12,
         prune = pruneBasic})
         )

testSearchFirstBFS1Xxboard1_2 :: Test
testSearchFirstBFS1Xxboard1_2 = TestCase $ assertEqual ""
        [board1, xxboard1]
        (head $ bfs xxboard1 2
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchFirstBFS1Basic_Inf :: Test
testSearchFirstBFS1Basic_Inf = TestCase $ assertEqual ""
        rightmost1
        (head $ bfs xboard1 Inf
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchFirstBFS1Xrank1_Inf :: Test
testSearchFirstBFS1Xrank1_Inf = TestCase $ assertEqual ""
        rightmost1
        (head $ bfs xboard1 Inf
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank11,
        prune = pruneBasic})
        )

testSearchFirstBFS1Xrank12_Inf :: Test
testSearchFirstBFS1Xrank12_Inf = TestCase $ assertEqual ""
        rightmost1
        (head $ bfs xboard1 Inf
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

testSearchFirstBFS1Xxboard1_Inf :: Test
testSearchFirstBFS1Xxboard1_Inf = TestCase $ assertEqual ""
        [board1, xxboard1]
        (head $ bfs xxboard1 Inf
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchBFS1GetSecond_11 :: Test
testSearchBFS1GetSecond_11 = TestCase $ assertEqual ""
        []
        (drop 1 $ bfs xxboard1 11
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

testSearchBFS1GetSecond_12 :: Test
testSearchBFS1GetSecond_12 = TestCase $ assertEqual ""
        [board1, xboard16, xboard15, xboard14, xboard13, xboard12, xboard11, xboard10, xboard9, xboard8, xboard7,xxboard1]
        (head $ drop 1 $ bfs xxboard1 12
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

testSearchBFSGetSecond_Inf :: Test
testSearchBFSGetSecond_Inf = TestCase $ assertEqual ""
        [board1, xboard16, xboard15, xboard14, xboard13, xboard12, xboard11, xboard10, xboard9, xboard8, xboard7,xxboard1]
        (head $ drop 1 $ bfs xxboard1 Inf
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

main :: IO Counts
main = runTestTT $ TestList [
        testSearchBFS0_11,
        testSearchBFS0_12,
        testSearchBFS0_13,
        testSearchBFS0,
        testSearchBFS0Fail_5,
        testSearchBFS0Fail_6,
        testSearchBFS0Fail,
        testSearchFirstBFS1Basic_4,
        testSearchFirstBFS1Xrank11_4,
        testSearchFirstBFS1Xrank12_4,
        testSearchFirstBFS1Xxboard1_2,
        testSearchFirstBFS1Basic_Inf,
        testSearchFirstBFS1Xrank1_Inf,
        testSearchFirstBFS1Xrank12_Inf,
        testSearchFirstBFS1Xxboard1_Inf,
        testSearchBFS1GetSecond_11,
        testSearchBFS1GetSecond_12,
        testSearchBFSGetSecond_Inf
        ]
