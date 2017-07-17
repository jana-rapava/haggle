module TestTSP where

import Test.HUnit
import TSP
import Data.Maybe (fromJust)

-------------------------------------------------
-- TESTCASE #0 - K_3, equilateral triangle
-------------------------------------------------

nbs_a = [('b',1), ('c',1)]
nbs_b = [('a',1), ('c',1)]
nbs_c = [('a',1), ('b',1)]

testGenerateNeighbours :: Test
testGenerateNeighbours = TestCase $ assertEqual ""
        [nbs_a, nbs_b, nbs_c]
        (generateNeighbours ['a','b','c'] [[1,1],[1,1],[1,1]])

graph0 = [ V { label = 'a', neighbours = nbs_a, visited = Expanded },
        V { label = 'b', neighbours = nbs_b, visited = Unvisited },
        V { label = 'c', neighbours = nbs_c, visited = Unvisited }]

testGenerateGraph :: Test
testGenerateGraph = TestCase $ assertEqual ""
        graph0
        (generateGraph ['a','b','c'] [[1,1],[1,1],[1,1]] 'a')

testFindExpanded :: Test
testFindExpanded = TestCase $ assertEqual ""
        0
        (fromJust $ findExpanded graph0)

testGetVertex :: Test
testGetVertex = TestCase $ assertEqual ""
        (V { label = 'a', neighbours = nbs_a, visited = Expanded }, 0)
        (getVertex graph0 'a')

testGetUnvisited :: Test
testGetUnvisited = TestCase $ assertEqual ""
        [1,2]
        (getUnvisited graph0 ['b','c'])

testComputeAdjacent :: Test
testComputeAdjacent = TestCase $ assertEqual ""
        [1,2]
        (computeAdjacent graph0 0)

testGenerateMoves :: Test
testGenerateMoves = TestCase $ assertEqual ""
        [M { posFrom = 0, posTo = 1 }, M { posFrom = 0, posTo = 2 }]
        (generateMoves graph0)

xgraph0 = [ V { label = 'a', neighbours = nbs_a, visited = Processed },
        V { label = 'b', neighbours = nbs_b, visited = Expanded },
        V { label = 'c', neighbours = nbs_c, visited = Unvisited }]

testApplyMove :: Test
testApplyMove = TestCase $ assertEqual ""
        xgraph0
        (applyMove graph0 (head $ generateMoves graph0))

xgraph1 = [ V { label = 'a', neighbours = nbs_a, visited = Processed },
        V { label = 'b', neighbours = nbs_b, visited = Unvisited },
        V { label = 'c', neighbours = nbs_c, visited = Expanded }]

testNextNeighbours :: Test
testNextNeighbours = TestCase $ assertEqual ""
        [xgraph0, xgraph1]
        (nextNeighbours graph0)
-------------------------------------------------
-- TESTCASE #1 - K_4, with triangle inequality
-------------------------------------------------
graph1 = generateGraph [0,1,2,3]
        [[],[],[],[]] 0
-------------------------------------------------
-- TESTCASE #2 - K_4, without triangle inequality
-------------------------------------------------
graph2 = generateGraph [0,1,2,3]
        [[],[],[],[]] 0
-------------------------------------------------
-- TESTCASE #3 - K_5, with triangle inequality
-------------------------------------------------
graph3 = generateGraph ["Lorem","ipsum","dolor","sit","amet"]
        [[],[],[],[],[]] "Lorem"

main :: IO Counts
main = runTestTT $ TestList [
        testGenerateNeighbours,
        testGenerateGraph,
        testFindExpanded,
        testGetVertex,
        testGetUnvisited,
        testComputeAdjacent,
        testGenerateMoves,
        testApplyMove,
        testNextNeighbours
        ]
