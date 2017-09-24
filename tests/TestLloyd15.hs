module TestLloyd15 where

import Test.HUnit
import Lloyd15
import TestFixtures
import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy, elemIndex, (\\))
import Data.Function (on)
import Control.Monad.Reader
import Control.Monad.State.Lazy

--------------
-- TESTCASE #0
--------------
-- boardHeight0 = 2
-- boardWidth0 = 2
-- cellVals0 = "ABC."
-- adjacent0 = [[1,2],[0,3],
--         [0,3],[1,2]]
-- blank0 = '.'
-- board0 = generateBoard blank0 boardHeight0 boardWidth0 cellVals0
-- board0' = M { blank = blank0,
--         height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'A'),(1,'.'),
--         (2,'C'),(3,'B')]}
-- aboard1 = M { blank = blank0,
--         height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'.'),(1,'A'),
--         (2,'C'),(3,'B')]}
-- aboard2 = M { blank = blank0,
--         height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'C'),(1,'A'),
--         (2,'.'),(3,'B')]}
-- aboard3 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'C'),(1,'A'),
--         (2,'B'),(3,'.')]}
-- aboard4 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'C'),(1,'.'),
--         (2,'B'),(3,'A')]}
-- aboard5 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'.'),(1,'C'),
--         (2,'B'),(3,'A')]}
-- aboard6 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'B'),(1,'C'),
--         (2,'.'),(3,'A')]}
-- aboard7 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'B'),(1,'C'),
--         (2,'A'),(3,'.')]}
-- aboard8 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'B'),(1,'.'),
--         (2,'A'),(3,'C')]}
-- aboard9 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'.'),(1,'B'),
--         (2,'A'),(3,'C')]}
-- aboard10 = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'A'),(1,'B'),
--         (2,'.'),(3,'C')]}
-- aaboard = M { blank = blank0,height = boardHeight0,
--         width = boardWidth0,
--         content = [(0,'B'),(1,'A'),
--         (2,'C'),(3,'.')]}
--
-- paths0 = [[board0, board0'], [board0, aboard10, aboard9, aboard8, aboard7, aboard6, aboard5, aboard4, aboard3, aboard2, aboard1, board0']]
-- paths0rev = [[board0, aboard10, aboard9, aboard8, aboard7, aboard6, aboard5, aboard4, aboard3, aboard2, aboard1, board0'], [board0, board0']]
-- stopSuccess0 = (== board0)
-- stopFail0 :: [Matrix a] -> Bool
-- stopFail0 = null


--------------
-- TESTCASE #1
--------------
-- boardHeight1 = 4
-- boardWidth1 = 4
-- cellvals1 = "ABCD\
--         \EFGH\
--         \IJKL\
--         \MNO."
-- adjacent1 = [[1,4],[0,2,5],[1,3,6],[2,7],
--         [0,5,8], [1,4,6,9], [2,5,7,10], [3,6,11],
--         [4,9,12], [5,8,10,13], [6,9,11,14], [7,10,15],
--         [8,13], [9,12,14], [10,13,15], [11, 14]]
-- blank1 = '.'
-- board1 = generateBoard blank1 boardHeight1 boardWidth1 cellvals1
-- swaps1 = [S {posFrom = 15, posTo = 11}, S {posFrom = 15, posTo = 14}]
-- board1' = M { blank = blank1,
--         height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'.'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard1 = M { blank = blank1, height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'.'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'D'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'H'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard2 = M {  blank = blank1,height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'.'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'H'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard3 = M {  blank = blank1,height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'.'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard4 = M {  blank = blank1,height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
--         (8,'I'), (9,'J'), (10,'.'), (11,'K'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard5 = M {  blank = blank1,height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
--         (4,'E'), (5,'F'), (6,'.'), (7,'G'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'H'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard6 = M {  blank = blank1,height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'.'), (3,'C'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'D'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'H'),
--         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
-- xboard7 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'.'),(11,'L'),
--         (12,'M'),(13,'N'),(14,'K'),(15,'O')]}
-- xboard8 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'L'),(11,'.'),
--         (12,'M'),(13,'N'),(14,'K'),(15,'O')]}
-- xboard9 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'L'),(11,'O'),
--         (12,'M'),(13,'N'),(14,'K'),(15,'.')]}
-- xboard10 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'L'),(11,'O'),
--         (12,'M'),(13,'N'),(14,'.'),(15,'K')]}
-- xboard11 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'.'),(11,'O'),
--         (12,'M'),(13,'N'),(14,'L'),(15,'K')]}
-- xboard12 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'O'),(11,'.'),
--         (12,'M'),(13,'N'),(14,'L'),(15,'K')]}
-- xboard13 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'O'),(11,'K'),
--         (12,'M'),(13,'N'),(14,'L'),(15,'.')]}
-- xboard14 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'O'),(11,'K'),
--         (12,'M'),(13,'N'),(14,'.'),(15,'L')]}
-- xboard15 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'.'),(11,'K'),
--         (12,'M'),(13,'N'),(14,'O'),(15,'L')]}
-- xboard16 = M { blank = blank1,height = 4,
--         width = 4,
--         content =
--         [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
--         (4,'E'),(5,'F'),(6,'G'),(7,'H'),
--         (8,'I'),(9,'J'),(10,'K'),(11,'.'),
--         (12,'M'),(13,'N'),(14,'O'),(15,'L')]}
-- xxboard1 = M {  blank = blank1,height = boardHeight1,
--         width = boardWidth1,
--         content =
--         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
--         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
--         (8,'I'), (9,'J'), (10,'K'), (11,'L'),
--         (12,'M'), (13, 'N'), (14,'.'), (15,'O')]}
-- nextBoards1 = [board1', xxboard1]
-- path1 = ([board1], 1)
-- paths1 = [([board1', board1],1), ([xxboard1, board1],2)]
-- paths2 = [([board1', board1],2), ([xxboard1, board1],2)]
-- paths3 = [([xboard14, board1], 8), ([xboard12, board1', board1], 5), ([xboard15,board1',board1], 6), ([xboard13, board1],3)]
-- paths4 = [([board1', board1],5), ([xxboard1, board1],7)]
-- paths5 = [([xboard13, board1],3),([xboard12, board1', board1], 5),([board1', board1],5),([xboard15,board1',board1], 6),([xxboard1, board1],7),([xboard14, board1], 8)]
-- xbacklog1 = [[xboard4], [xboard5], [xboard6],[]]
-- rightmost1 = [board1, xboard3, xboard2, xboard1]
-- stopSuccess1 = (== board1)
-- stopFail1 :: [Matrix a] -> Bool
-- stopFail1 = null
--
-- xrank11 b _ = misplaced board1 b
-- xrank12 b _ = manhattan_sum boardHeight1 boardWidth1 board1 b

testGenerateBoard1 :: Test
testGenerateBoard1 = TestCase $ assertEqual ""
        [0..boardHeight1*boardWidth1-1]
        (map fst $ content $ generateBoard blank1 boardHeight1 boardWidth1 cellvals1)

testGenerateBoard2 :: Test
testGenerateBoard2 = TestCase $ assertEqual ""
        cellvals1
        (map snd $ content $ generateBoard blank1 boardHeight1 boardWidth1 cellvals1)

testComputeAdjacent1 :: Test
testComputeAdjacent1 = TestCase $ assertEqual ""
        adjacent1 (computeAdjacent boardHeight1 boardWidth1)

testGenerateSwaps1 :: Test
testGenerateSwaps1 = TestCase $ assertEqual "chars"
        swaps1 (generateSwaps board1)

testApplySwap1 :: Test
testApplySwap1 = TestCase $ assertEqual ""
        board1' (applySwap board1 (head swaps1))

testApplySwap2 :: Test
testApplySwap2 = TestCase $ assertEqual ""
        xboard2 (applySwap xboard1 (S { posFrom = 3, posTo = 7}))

testNextBoards1 :: Test
testNextBoards1 = TestCase $ assertEqual ""
        nextBoards1 (nextBoards board1)

--------------
-- TESTCASE #2
--------------
-- boardHeight2 = 2
-- boardWidth2 = 7
-- cellvals2 = [1, 2, 3, 4, 5, 6, 7,
--         8, 9, 10, 11, 12, 13, 0]
-- adjacent2 = [[1,7],[0,2,8],[1,3,9],[2,4,10],[3,5,11],[4,6,12],[5,13],
--         [0,8],[1,7,9],[2,8,10],[3,9,11],[4,10,12],[5,11,13],[6,12]]
-- blank2 = 0
-- board2 = generateBoard blank2 boardHeight2 boardWidth2 cellvals2
-- swaps2 = [S {posFrom = 13, posTo = 6}, S {posFrom = 13, posTo = 12}]
-- board2' = M { blank = blank2,
--         height = boardHeight2,
--         width = boardWidth2,
--         content =
--         [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,0),
--         (7,8),(8,9), (9,10), (10,11), (11,12),(12,13), (13,7)]}
-- yboard1 = M { blank = blank2, height = boardHeight2,
--         width = boardWidth2,
--         content =
--         [(0,1), (1,2), (2,3), (3,0),(4,5), (5,6), (6,7),
--         (7,8),(8,9), (9,10), (10,4), (11,11),(12,12), (13,13)]}
-- yboard2 = M { blank = blank2, height = boardHeight2,
--         width = boardWidth2,
--         content =
--         [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
--         (7,8),(8,9), (9,10), (10,0), (11,11),(12,12), (13,13)]}
-- yboard3 = M { blank = blank2, height = boardHeight2,
--         width = boardWidth2,
--         content =
--         [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
--         (7,8),(8,9), (9,10), (10,11), (11,0),(12,12), (13,13)]}
-- yboard4 = M { blank = blank2, height = boardHeight2,
--         width = boardWidth2,
--         content =
--         [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
--         (7,8),(8,9), (9,10), (10,11), (11,12),(12,0), (13,13)]}
-- nextBoards2 = [[(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,0),
--         (7,8),(8,9), (9,10), (10,11), (11,12),(12,13), (13,7)],
--         [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
--         (7,8),(8,9), (9,10), (10,11), (11,12),(12,0), (13,13)]]
-- rightmost2 = [board2, yboard4, yboard3, yboard2, yboard1]
-- stopSuccess2 = (== board2)
-- stopFail2 :: [Matrix a] -> Bool
-- stopFail2 = null
--
-- xrank21 b _ = misplaced board2 b
-- xrank22 b _ = manhattan_sum boardHeight2 boardWidth2 board2 b
--
-- testGenerateBoard3 :: Test
-- testGenerateBoard3 = TestCase $ assertEqual ""
--         [0..boardHeight2*boardWidth2-1]
--         (map fst $ content $ generateBoard blank2 boardHeight2 boardWidth2 cellvals2)
--
-- testGenerateBoard4 :: Test
-- testGenerateBoard4 = TestCase $ assertEqual ""
--         cellvals2
--         (map snd $ content $ generateBoard blank2 boardHeight2 boardWidth2 cellvals2)
--
-- testComputeAdjacent2 :: Test
-- testComputeAdjacent2 = TestCase $ assertEqual ""
--         adjacent2 (computeAdjacent boardHeight2 boardWidth2)
--
-- testGenerateSwaps2 :: Test
-- testGenerateSwaps2 = TestCase $ assertEqual "nums"
--         swaps2 (generateSwaps board2 )
--
-- testApplySwap3 :: Test
-- testApplySwap3 = TestCase $ assertEqual ""
--         board2' (applySwap board2 (head swaps2))
--
-- testNextBoards2 :: Test
-- testNextBoards2 = TestCase $ assertEqual ""
--         nextBoards2 (map content $ nextBoards board2)
--
--
-- --------------
-- -- TESTCASE #3
-- --------------
-- boardHeight3 = 5
-- boardWidth3 = 3
-- cellvals3 = ["Lorem", "ipsum", "dolor",
--         "sit", "amet", "consectetur",
--         "adipiscing", "elit", "sed",
--         "do", "eiusmod", "tempor",
--         "incididunt", "ut",""]
-- adjacent3 = [[1,3],[0,2,4],[1,5],
--         [0,4,6],[1,3,5,7],[2,4,8],
--         [3,7,9],[4,6,8,10],[5,7,11],
--         [6,10,12],[7,9,11,13],[8,10,14],
--         [9,13],[10,12,14],[11,13]]
-- blank3 = ""
-- board3 = generateBoard blank3 boardHeight3 boardWidth3 cellvals3
-- swaps3 = [S {posFrom = 14, posTo = 11}, S {posFrom = 14, posTo = 13}]
-- board3' = M { blank =  blank3,
--         height = boardHeight3,
--         width = boardWidth3,
--         content =
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,"adipiscing"), (7,"elit"), (8, "sed"),
--         (9,"do"), (10,"eiusmod"), (11,"") ,
--         (12,"incididunt"), (13,"ut"),(14,"tempor")]}
-- zboard1 = M {  blank =  blank3,height = boardHeight3,
--         width = boardWidth3,
--         content =
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,""), (4,"amet"), (5,"consectetur"),
--         (6,"sit"), (7,"elit"), (8, "sed"),
--         (9,"adipiscing"), (10,"eiusmod"), (11,"tempor") ,
--         (12,"do"), (13,"incididunt"),(14,"ut")]}
-- zboard2 = M {  blank =  blank3,height = boardHeight3,
--         width = boardWidth3,
--         content =
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,""), (7,"elit"), (8, "sed"),
--         (9,"adipiscing"), (10,"eiusmod"), (11,"tempor") ,
--         (12,"do"), (13,"incididunt"), (14,"ut")]}
-- zboard3 = M {  blank =  blank3,height = boardHeight3,
--         width = boardWidth3,
--         content =
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,"adipiscing"), (7,"elit"), (8, "sed"),
--         (9,""), (10,"eiusmod"), (11,"tempor") ,
--         (12,"do"), (13,"incididunt"), (14,"ut")]}
-- zboard4 = M {  blank =  blank3,height = boardHeight3,
--         width = boardWidth3,
--         content =
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,"adipiscing"), (7,"elit"), (8, "sed"),
--         (9,"do"), (10,"eiusmod"), (11,"tempor") ,
--         (12,""),(13,"incididunt"), (14,"ut")]}
-- zboard5 = M {  blank =  blank3,height = boardHeight3,
--         width = boardWidth3,
--         content =
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,"adipiscing"), (7,"elit"), (8, "sed"),
--         (9,"do"), (10,"eiusmod"), (11,"tempor") ,
--         (12,"incididunt"), (13,""),(14,"ut")]}
-- nextBoards3 = [[(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,"adipiscing"), (7,"elit"), (8, "sed"),
--         (9,"do"), (10,"eiusmod"), (11,"") ,
--         (12,"incididunt"), (13,"ut"),(14,"tempor")],
--         [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
--         (3,"sit"), (4,"amet"), (5,"consectetur"),
--         (6,"adipiscing"), (7,"elit"), (8, "sed"),
--         (9,"do"), (10,"eiusmod"), (11,"tempor") ,
--         (12,"incididunt"), (13,""),(14,"ut")]]
-- rightmost3 = [board3, zboard5, zboard4, zboard3, zboard2, zboard1]
-- stopSuccess3 = (== board3)
-- stopFail3 :: [Matrix a] -> Bool
-- stopFail3 = null
--
-- xrank31 b _ = misplaced board3 b
-- xrank32 b _ = manhattan_sum boardHeight3 boardWidth3 board3 b
--
-- testGenerateBoard5 :: Test
-- testGenerateBoard5 = TestCase $ assertEqual ""
--         [0..boardHeight3*boardWidth3-1]
--         (map fst $ content $ generateBoard blank3 boardHeight3 boardWidth3 cellvals3)
--
-- testGenerateBoard6 :: Test
-- testGenerateBoard6 = TestCase $ assertEqual ""
--         cellvals3
--         (map snd $ content $ generateBoard blank3 boardHeight3 boardWidth3 cellvals3)
--
-- testComputeAdjacent3 :: Test
-- testComputeAdjacent3 = TestCase $ assertEqual ""
--         adjacent3 (computeAdjacent boardHeight3 boardWidth3)
--
-- testGenerateSwaps3 :: Test
-- testGenerateSwaps3 = TestCase $ assertEqual "strings"
--         swaps3 (generateSwaps board3)
--
-- testApplySwap4 :: Test
-- testApplySwap4 = TestCase $ assertEqual ""
--         board3' (applySwap board3 (head swaps3))
--
-- testNextBoards3 :: Test
-- testNextBoards3 = TestCase $ assertEqual ""
--         nextBoards3 (map content $ nextBoards board3)

main :: IO Counts
main = runTestTT $ TestList [
        testGenerateBoard1,
        testGenerateBoard2,
--         testGenerateBoard3,
--         testGenerateBoard4,
--         testGenerateBoard5,
--         testGenerateBoard6,
        testComputeAdjacent1,
--         testComputeAdjacent2,
--         testComputeAdjacent3,
        testGenerateSwaps1,
--         testGenerateSwaps2,
--         testGenerateSwaps3,
        testApplySwap1,
        testApplySwap2,
--         testApplySwap3,
--         testApplySwap4,
        testNextBoards1
--         testNextBoards2,
--         testNextBoards3,
--         testSearchFirstDFS0,
--         testSearchFirstDFS1,
--         testSearchFirstDFS2,
--         testSearchFirstDFS3,
--         testSearchFirstDFS4,
--         testSearchFirstDFS5,
--         testSearchFirstDFS6,
--         testSearchFirstDFS7,
--         testSearchFirstDFS8,
--         testSearchFirstDFS9,
--         testSearchFirstDFS10,
--         testSearchFirstBFS11,
--         testSearchFirstBFS12,
--         testSearchFirstBFS13,
--         testSearchFirstBFS14,
--         testSearchFirstBFS15,
--         testSearchFirstBFS16,
--         testSearchFirstBFS17,
--         testSearchFirstBFS18,
--         testSearchFirstBeFS0,
--         testSearchFirstBeFS1,
--         testSearchFirstBeFS2,
--         testSearchFirstBeFS3,
--         testSearchFirstBeFS4,
--         testSearchFirstBeFS5,
--         testSearchFirstBeFS6,
--         testSearchFirstBeFS7,
--         testSearchFirstBeFS8,
--         testSearchFirstBeFS9,
--         testSearchFirstBeFS10,
--         testSearchDFS0,
--        testSearchDFS0_2,
--         testSearchDFS0Fail,
--         testSearchDFS1,
--         testSearchBeFS0,
--        testSearchBeFS0_2,
--         testSearchBeFS0Fail,
--         testSearchBeFS1,
--         testSearchBFS2_11,
--         testSearchBFS2_12,
--         testSearchBFS2_13,
--         testSearchBFS2,
--         testSearchBFS2Fail,
--         testSearchBFS2Fail_5,
--         testSearchBFS2Fail_6,
--         testSearchBFS3_11,
--         testSearchBFS3_12,
--         testSearchBFS3
        ]
