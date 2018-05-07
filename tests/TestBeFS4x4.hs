{-# LANGUAGE FlexibleInstances #-}

module TestBeFS4x4 where

import Test.HUnit
import BeFS
-- import FunctionStore
import TestFixtures
import Lloyd15
import Expandable

success = "ABCD\
           \EFGH\
           \IJKL\
           \MNO."

successBoard = generateBoard '.' 4 4 success

instance Expandable (Matrix Char) where
        stopSuccess = (== successBoard)
        generateNbs = nextBoards

--------------------
-- TESTCASE #1
--------------------

-- this test is supposed to run long (cca 5mins) to show that the next one really uses provided heuristics
testSearchFirstBeFS1yBasic :: Test
testSearchFirstBeFS1yBasic = TestCase $ assertBool "solution should exist, but not found"
        (not $ null $ testBefs board1y rankBasic)

testSearchFirstBeFS1ymisplaced :: Test
testSearchFirstBeFS1ymisplaced = TestCase $ assertEqual ""
        rightmost1
        (head $ testBefs board1y misplaced)

testSearchFirstBeFS1ymanhattan_sum :: Test
testSearchFirstBeFS1ymanhattan_sum = TestCase $ assertEqual ""
        rightmost1
        (head $ testBefs board1y manhattan_sum)

-- this test is supposed to run long (cca 5mins) to show that the next one really uses provided heuristics
testSearchFirstBeFS1xBasic :: Test
testSearchFirstBeFS1xBasic = TestCase $ assertBool "solution should exist, but not found"
         (not $ null $ testBefs board1x rankBasic)

testSearchFirstBeFS1xmanhattan_sum :: Test
testSearchFirstBeFS1xmanhattan_sum = TestCase $ assertEqual ""
        [board1, board1x]
         (head $ testBefs board1x manhattan_sum)

testSearchSecondBeFS1xmisplaced :: Test
testSearchSecondBeFS1xmisplaced = TestCase $ assertEqual ""
        [board1, board1w, board1v, board1u, board1t, board1s, board1r, board1q, board1p, board1o, board1n, board1x]
        (head $ drop 1 $ testBefs board1x misplaced)

main :: IO Counts
main = runTestTT $ TestList [
                testSearchFirstBeFS1yBasic,
                testSearchFirstBeFS1ymisplaced,
                testSearchFirstBeFS1ymanhattan_sum,
                testSearchFirstBeFS1xBasic,
                testSearchFirstBeFS1xmanhattan_sum,
                testSearchSecondBeFS1xmisplaced
                ]