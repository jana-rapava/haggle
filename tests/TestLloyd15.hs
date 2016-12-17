module TestLloyd15 where

import Test.HUnit
import Lloyd15

boardHeight1 = 4
boardWidth1 = 4
cellvals1 = "ABCD\
        \EFGH\
        \IJKL\
        \MNO."
adjacent1 = [[1,4],[0,2,5],[1,3,6],[2,7],
        [0,5,8], [1,4,6,9], [2,5,7,10], [3,6,11],
        [4,9,12], [5,8,10,13], [6,9,11,14], [7,10,15],
        [8,13], [9,12,14], [10,13,15], [11, 14]]
board1 = generateBoard boardHeight1 boardWidth1 cellvals1
swaps1 = [S {posFrom = 15, sym = 'L', posTo = 11}, S {posFrom = 15, sym = 'O', posTo = 14}]
blank1 = (=='.')

testGenerateBoard1 :: Test
testGenerateBoard1 = TestCase $ assertEqual ""
        [0..boardHeight1*boardWidth1-1]
        (map fst $ generateBoard boardHeight1 boardWidth1 cellvals1)

testGenerateBoard2 :: Test
testGenerateBoard2 = TestCase $ assertEqual ""
        cellvals1
        (map snd $ generateBoard boardHeight1 boardWidth1 cellvals1)

testComputeAdjacent1 :: Test
testComputeAdjacent1 = TestCase $ assertEqual ""
        adjacent1 (computeAdjacent boardHeight1 boardWidth1)

testGenerateSwaps1 :: Test
testGenerateSwaps1 = TestCase $ assertEqual "chars"
        swaps1 (generateSwaps boardHeight1 boardWidth1 board1 blank1 )

boardHeight2 = 2
boardWidth2 = 7
cellvals2 = [1, 2, 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 0]
adjacent2 = [[1,7],[0,2,8],[1,3,9],[2,4,10],[3,5,11],[4,6,12],[5,13],
        [0,8],[1,7,9],[2,8,10],[3,9,11],[4,10,12],[5,11,13],[6,12]]
board2 = generateBoard boardHeight2 boardWidth2 cellvals2
swaps2 = [S {posFrom = 13, sym = 7, posTo = 6}, S {posFrom = 13, sym = 13, posTo = 12}]
blank2 = (==0)

testGenerateBoard3 :: Test
testGenerateBoard3 = TestCase $ assertEqual ""
        [0..boardHeight2*boardWidth2-1]
        (map fst $ generateBoard boardHeight2 boardWidth2 cellvals2)

testGenerateBoard4 :: Test
testGenerateBoard4 = TestCase $ assertEqual ""
        cellvals2
        (map snd $ generateBoard boardHeight2 boardWidth2 cellvals2)

testComputeAdjacent2 :: Test
testComputeAdjacent2 = TestCase $ assertEqual ""
        adjacent2 (computeAdjacent boardHeight2 boardWidth2)

testGenerateSwaps2 :: Test
testGenerateSwaps2 = TestCase $ assertEqual "nums"
        swaps2 (generateSwaps boardHeight2 boardWidth2 board2 blank2)

boardHeight3 = 5
boardWidth3 = 3
cellvals3 = ["Lorem", "ipsum", "dolor",
        "sit", "amet", "consectetur",
        "adipiscing", "elit", "sed",
        "do", "eiusmod", "tempor",
        "incididunt", "ut",""]
adjacent3 = [[1,3],[0,2,4],[1,5],
        [0,4,6],[1,3,5,7],[2,4,8],
        [3,7,9],[4,6,8,10],[5,7,11],
        [6,10,12],[7,9,11,13],[8,10,14],
        [9,13],[10,12,14],[11,13]]
board3 = generateBoard boardHeight3 boardWidth3 cellvals3
swaps3 = [S {posFrom = 14, sym = "tempor", posTo = 11}, S {posFrom = 14, sym = "ut", posTo = 13}]
blank3 = (=="")

testGenerateBoard5 :: Test
testGenerateBoard5 = TestCase $ assertEqual ""
        [0..boardHeight3*boardWidth3-1]
        (map fst $ generateBoard boardHeight3 boardWidth3 cellvals3)

testGenerateBoard6 :: Test
testGenerateBoard6 = TestCase $ assertEqual ""
        cellvals3
        (map snd $ generateBoard boardHeight3 boardWidth3 cellvals3)

testComputeAdjacent3 :: Test
testComputeAdjacent3 = TestCase $ assertEqual ""
        adjacent3 (computeAdjacent boardHeight3 boardWidth3)

testGenerateSwaps3 :: Test
testGenerateSwaps3 = TestCase $ assertEqual "strings"
        swaps3 (generateSwaps boardHeight3 boardWidth3 board3 blank3)

main :: IO Counts
main = runTestTT $ TestList [
        testGenerateBoard1,
        testGenerateBoard2,
        testGenerateBoard3,
        testGenerateBoard4,
        testGenerateBoard5,
        testGenerateBoard6,
        testComputeAdjacent1,
        testComputeAdjacent2,
        testComputeAdjacent3,
        testGenerateSwaps1,
        testGenerateSwaps2,
        testGenerateSwaps3
         ]
