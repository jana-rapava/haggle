module TestLloyd15 where

import Test.HUnit
import Lloyd15
import TestFixtures

----------------------
-- TESTCASE #0 - tests
----------------------
testGenerateBoard0a :: Test
testGenerateBoard0a = TestCase $ assertEqual ""
        [0..boardHeight0*boardWidth0-1]
        (map fst $ content $ generateBoard blank0 boardHeight0 boardWidth0 cellvals0)

testGenerateBoard0b :: Test
testGenerateBoard0b = TestCase $ assertEqual ""
        cellvals0
        (map snd $ content $ generateBoard blank0 boardHeight0 boardWidth0 cellvals0)

testComputeAdjacent0 :: Test
testComputeAdjacent0 = TestCase $ assertEqual ""
        adjacent0 (computeAdjacent boardHeight0 boardWidth0)

testNextBoards0 :: Test
testNextBoards0 = TestCase $ assertEqual ""
        nextBoards0 (nextBoards board0)

-----------------------
-- TESTCASE #1 - tests
-----------------------
testGenerateBoard1a :: Test
testGenerateBoard1a = TestCase $ assertEqual ""
        [0..boardHeight1*boardWidth1-1]
        (map fst $ content $ generateBoard blank1 boardHeight1 boardWidth1 cellvals1)

testGenerateBoard1b :: Test
testGenerateBoard1b = TestCase $ assertEqual ""
        cellvals1
        (map snd $ content $ generateBoard blank1 boardHeight1 boardWidth1 cellvals1)

testComputeAdjacent1 :: Test
testComputeAdjacent1 = TestCase $ assertEqual ""
        adjacent1 (computeAdjacent boardHeight1 boardWidth1)

testGenerateSwaps1 :: Test
testGenerateSwaps1 = TestCase $ assertEqual "chars"
        swaps1 (generateSwaps board1)

testApplySwap1a :: Test
testApplySwap1a = TestCase $ assertEqual ""
        board1a (applySwap board1 (head swaps1))

testApplySwap1b :: Test
testApplySwap1b = TestCase $ assertEqual ""
        board1b (applySwap board1b' (S { posFrom = 3, posTo = 7}))

testNextBoards1a :: Test
testNextBoards1a = TestCase $ assertEqual ""
        nextBoards1a (nextBoards board1a)

testNextBoards1b :: Test
testNextBoards1b = TestCase $ assertEqual ""
        nextBoards1b (nextBoards board1d)

main :: IO Counts
main = runTestTT $ TestList [
        testGenerateBoard0a,
        testGenerateBoard0b,
        testComputeAdjacent0,
        testNextBoards0,
        testGenerateBoard1a,
        testGenerateBoard1b,
        testComputeAdjacent1,
        testGenerateSwaps1,
        testApplySwap1a,
        testApplySwap1b,
        testNextBoards1a,
        testNextBoards1b
        ]
