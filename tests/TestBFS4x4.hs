{-# LANGUAGE FlexibleInstances #-}

module TestBFS4x4 where

import Test.HUnit
import BFS
import InfInt
--import FunctionStore
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

---------------
-- TESTCASE #0
---------------
testSearchFirstBFS1Basic_4 :: Test
testSearchFirstBFS1Basic_4 = TestCase $ assertEqual ""
         rightmost1
         (head $ testBfs board1y 4)

testSearchFirstBFS1Xxboard1_2 :: Test
testSearchFirstBFS1Xxboard1_2 = TestCase $ assertEqual ""
        [board1, board1x]
        (head $ testBfs board1x 2)

testSearchFirstBFS1Basic_Inf :: Test
testSearchFirstBFS1Basic_Inf = TestCase $ assertEqual ""
        rightmost1
        (head $ testBfs board1y Inf)

testSearchFirstBFS1Xxboard1_Inf :: Test
testSearchFirstBFS1Xxboard1_Inf = TestCase $ assertEqual ""
        [board1, board1x]
        (head $ testBfs board1x Inf)

main :: IO Counts
main = runTestTT $ TestList [
        testSearchFirstBFS1Basic_4,
        testSearchFirstBFS1Xxboard1_2,
        testSearchFirstBFS1Basic_Inf,
        testSearchFirstBFS1Xxboard1_Inf--,
        ]
