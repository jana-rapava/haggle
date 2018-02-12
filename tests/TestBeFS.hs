module TestBeFS where

import Test.HUnit
import BeFS
-- import FunctionStore
import TestFixtures

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

--------------------
-- TESTCASE #1
--------------------

testSearchFirstBeFS1hBasic :: Test
testSearchFirstBeFS1hBasic = TestCase $ assertEqual ""
        rightmost1
        (head $ testBefs board1h rankBasic)

testSearchFirstBeFS1hmisplaced :: Test
testSearchFirstBeFS1hmisplaced = TestCase $ assertEqual ""
        rightmost1
        (head $ testBefs board1h misplaced)

testSearchFirstBeFS1hmanhattan_sum :: Test
testSearchFirstBeFS1hmanhattan_sum = TestCase $ assertEqual ""
        rightmost1
        (head $ testBefs board1h manhattan_sum)

testSearchFirstBeFS1xBasic :: Test
testSearchFirstBeFS1xBasic = TestCase $ assertEqual ""
         [board1, board1x]
         (head $ testBefs board1x rankBasic)

testSearchSecondBeFS1xmanhattan_sum :: Test
testSearchSecondBeFS1xmanhattan_sum = TestCase $ assertEqual ""
        [board1, board1w, board1v, board1u, board1t, board1s, board1r, board1q, board1p, board1o, board1n, board1x]
        (head $ drop 1 $ testBefs board1x manhattan_sum)

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
