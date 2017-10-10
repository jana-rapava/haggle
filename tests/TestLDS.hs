module TestLDS where

import Test.HUnit
import LDS
import FunctionStore
import TestFixtures

---------------
-- TESTCASE #0
---------------
testSearchFirstLDS0 :: Test
testSearchFirstLDS0 = TestCase $ assertEqual ""
        [board0, board0']
        (head $ lds board0'
--        (fromJust $ searchFirst board0' blank0
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )


testSearchLDS0Basic :: Test
testSearchLDS0Basic = TestCase $ assertEqual ""
        paths0
        (lds board0'
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

-- testSearchLDS0Basic2 :: Test
-- testSearchLDS0_2 = TestCase $ assertEqual ""
--         paths0rev
--         (dfs board0'
--         (FS {
--         stopSuccess = stopSuccess0,
--         stopFail = stopFail0,
--         rank = rankBasic2,
--         prune = pruneBasic})
--         )

testSearchLDS0Fail :: Test
testSearchLDS0Fail = TestCase $ assertEqual ""
        []
        (lds aaboard
        (FS {
        stopSuccess = stopSuccess0,
        stopFail = stopFail0,
        rank = rankBasic,
        prune = pruneBasic})
        )

--------------------
-- TESTCASE #1
--------------------

testSearchFirstLDS1Basic :: Test
testSearchFirstLDS1Basic = TestCase $ assertEqual ""
        rightmost1
        (head $ lds xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = rankBasic,
        prune = pruneBasic})
        )

testSearchFirstLDS1Xrank11 :: Test
testSearchFirstLDS1Xrank11 = TestCase $ assertEqual ""
        rightmost1
        (head $ lds xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank11,
        prune = pruneBasic})
        )

testSearchFirstLDS1Xrank12 :: Test
testSearchFirstLDS1Xrank12 = TestCase $ assertEqual ""
        rightmost1
        (head $ lds xboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

testSearchFirstLDS1Xxboard1 :: Test
testSearchFirstLDS1Xxboard1 = TestCase $ assertEqual ""
         [board1, xxboard1]
         (head $ lds xxboard1
         (FS {
         stopSuccess = stopSuccess1,
         stopFail = stopFail1,
         rank = rankBasic,
         prune = pruneBasic})
         )

testSearchLDS1GetSecond :: Test
testSearchLDS1GetSecond = TestCase $ assertEqual ""
        [board1, xboard16, xboard15, xboard14, xboard13, xboard12, xboard11, xboard10, xboard9, xboard8, xboard7,xxboard1]
        (head $ drop 1 $ lds xxboard1
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        rank = xrank12,
        prune = pruneBasic})
        )

main :: IO Counts
main = runTestTT $ TestList [
        ]
