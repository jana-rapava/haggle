module TestFixtures where

import Lloyd15
import Data.List ((\\), elemIndex)
import Data.Maybe (fromJust)
import BeFS (SortedList(..))

-- this function determines the search order - change this to implement different heuristics
rankBasic :: Matrix a -> Integer -> Integer
rankBasic _ i = i

-- this function prunes the branches of search space
pruneBasic :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
-- delete all items in l1 which appear in l2
pruneBasic = (\\)

-- heuristics 1: rank the board with lowest number of misplaced tiles
misplaced :: (Eq a) => Matrix a -> Matrix a -> Integer
misplaced b1 b2 = fromIntegral $ length $ filter (== False) $ zipWith (==) (content b1) (content b2)

-- heuristics 2: rank the board with lowest sum of Manhattan distances from the goal configuration
manhattan :: (Eq a) => Int -> Int -> Matrix a -> (Int, a) -> Int
manhattan height width b (n,x) = abs (row n - row m) + abs (column n - column m)
        where
                m = fromJust $ elemIndex x (map snd $ content b)
                row x = x `div` width
                column x = x `mod` height

manhattan_sum :: (Eq a) => Int -> Int -> Matrix a -> Matrix a -> Integer
manhattan_sum height width b1 b2 = fromIntegral $ sum $ map (manhattan height width b1) (content b2)

--------------
-- TESTCASE #0
--------------

boardHeight0 = 2
boardWidth0 = 2
cellVals0 = "ABC."
adjacent0 = [[1,2],[0,3],
        [0,3],[1,2]]
blank0 = '.'
board0 = generateBoard blank0 boardHeight0 boardWidth0 cellVals0
board0' = M { blank = blank0,
        height = boardHeight0,
        width = boardWidth0,
        content = [(0,'A'),(1,'.'),
        (2,'C'),(3,'B')]}
aboard1 = M { blank = blank0,
        height = boardHeight0,
        width = boardWidth0,
        content = [(0,'.'),(1,'A'),
        (2,'C'),(3,'B')]}
aboard2 = M { blank = blank0,
        height = boardHeight0,
        width = boardWidth0,
        content = [(0,'C'),(1,'A'),
        (2,'.'),(3,'B')]}
aboard3 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'C'),(1,'A'),
        (2,'B'),(3,'.')]}
aboard4 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'C'),(1,'.'),
        (2,'B'),(3,'A')]}
aboard5 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'.'),(1,'C'),
        (2,'B'),(3,'A')]}
aboard6 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'C'),
        (2,'.'),(3,'A')]}
aboard7 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'C'),
        (2,'A'),(3,'.')]}
aboard8 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'.'),
        (2,'A'),(3,'C')]}
aboard9 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'.'),(1,'B'),
        (2,'A'),(3,'C')]}
aboard10 = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'A'),(1,'B'),
        (2,'.'),(3,'C')]}
aaboard = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'A'),
        (2,'C'),(3,'.')]}

path0 = [board0, aboard10, aboard9, aboard8, aboard7, aboard6, aboard5, aboard4, aboard3, aboard2, aboard1, board0']
paths0 = [path0, [board0, board0']]
paths0rev = [[board0, aboard10, aboard9, aboard8, aboard7, aboard6, aboard5, aboard4, aboard3, aboard2, aboard1, board0'], [board0, board0']]
stopSuccess0 = (== board0)
stopFail0 :: [Matrix a] -> Bool
stopFail0 = null

--------------
-- TESTCASE #1
--------------

boardHeight1 = 4
boardWidth1 = 4
cellvals1 = "ABCD\
        \EFGH\
        \IJKL\
        \MNO."
adjacent1 :: [[Int]]
adjacent1 = [[1,4],[0,2,5],[1,3,6],[2,7],
        [0,5,8], [1,4,6,9], [2,5,7,10], [3,6,11],
        [4,9,12], [5,8,10,13], [6,9,11,14], [7,10,15],
        [8,13], [9,12,14], [10,13,15], [11, 14]]
blank1 = '.'
board1 = generateBoard blank1 boardHeight1 boardWidth1 cellvals1
swaps1 = [S {posFrom = 15, posTo = 11}, S {posFrom = 15, posTo = 14}]
board1' = M { blank = blank1,
        height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard1 = M { blank = blank1, height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'.'),
        (4,'E'), (5,'F'), (6,'G'), (7,'D'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard2 = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'.'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard3 = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard4 = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'.'), (11,'K'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard5 = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'.'), (7,'G'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard6 = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'.'), (3,'C'),
        (4,'E'), (5,'F'), (6,'G'), (7,'D'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard7 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'.'),(11,'L'),
        (12,'M'),(13,'N'),(14,'K'),(15,'O')]}
xboard8 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'L'),(11,'.'),
        (12,'M'),(13,'N'),(14,'K'),(15,'O')]}
xboard9 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'L'),(11,'O'),
        (12,'M'),(13,'N'),(14,'K'),(15,'.')]}
xboard10 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'L'),(11,'O'),
        (12,'M'),(13,'N'),(14,'.'),(15,'K')]}
xboard11 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'.'),(11,'O'),
        (12,'M'),(13,'N'),(14,'L'),(15,'K')]}
xboard12 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'O'),(11,'.'),
        (12,'M'),(13,'N'),(14,'L'),(15,'K')]}
xboard13 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'O'),(11,'K'),
        (12,'M'),(13,'N'),(14,'L'),(15,'.')]}
xboard14 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'O'),(11,'K'),
        (12,'M'),(13,'N'),(14,'.'),(15,'L')]}
xboard15 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'.'),(11,'K'),
        (12,'M'),(13,'N'),(14,'O'),(15,'L')]}
xboard16 = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'K'),(11,'.'),
        (12,'M'),(13,'N'),(14,'O'),(15,'L')]}
xxboard1 = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'L'),
        (12,'M'), (13, 'N'), (14,'.'), (15,'O')]}
nextBoards1 = [board1', xxboard1]
path1 :: ([Matrix Char], Integer)
path1 = ([board1], 1)
paths1 :: [([Matrix Char], Integer)]
paths1 = [([board1', board1],0), ([xxboard1, board1],0)]
paths2 :: [([Matrix Char], Integer)]
paths2 = [([board1', board1],2), ([xxboard1, board1],2)]
paths3 :: SortedList ([Matrix Char], Integer)
paths3 = SortedList [([xboard13, board1],3), ([xboard12, board1', board1], 5), ([xboard15,board1',board1], 6), ([xboard14, board1], 8) ]
--paths3 = SortedList [([xboard12, board1', board1], 5)]
paths4 :: [([Matrix Char], Integer)]
--paths4 = [([xboard14, board1], 8), ([xboard13, board1],3)]
paths4 = [([board1', board1],5), ([xxboard1, board1],7)]
paths5 :: SortedList ([Matrix Char], Integer)
--paths5 = SortedList [([xboard13, board1],3), ([xboard12, board1', board1], 5), ([xboard14, board1], 8)]
paths5 = SortedList [([xboard13, board1],3),([board1', board1],5),([xboard12, board1', board1], 5),([xboard15,board1',board1], 6),([xxboard1, board1],7),([xboard14, board1], 8)]
xbacklog1 = [[xboard4], [xboard5], [xboard6],[]]
rightmost1 = [board1, xboard3, xboard2, xboard1]
stopSuccess1 = (== board1)
stopFail1 :: [Matrix a] -> Bool
stopFail1 = null

xrank11 b _ = misplaced board1 b
xrank12 b _ = manhattan_sum boardHeight1 boardWidth1 board1 b
