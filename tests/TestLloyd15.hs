module TestLloyd15 where

import Test.HUnit
import Lloyd15
import TestFixtures

--------------
-- TESTCASE #0 - tests
---------------
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
-- TESTCASE #1 - tests
--------------
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
