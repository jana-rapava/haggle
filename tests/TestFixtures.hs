module TestFixtures where

import Haggle
import Lloyd15
import Data.List ((\\), elemIndex)
import Data.Maybe (fromJust)
import Path

-- this function determines the search order - change this to implement different heuristics
rankBasic :: Matrix a -> Int
rankBasic _ = 0

-- this function prunes the branches of search space
--pruneBasic :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
-- delete all items in l1 which appear in l2
--pruneBasic = (\\)

-- heuristics 1: rank the board with lowest number of misplaced tiles
misplaced' :: (Eq a) => Matrix a -> Matrix a -> Int
misplaced' b1 b2 = fromIntegral $ length $ filter (== False) $ zipWith (==) (content b1) (content b2)

-- heuristics 2: rank the board with lowest sum of Manhattan distances from the goal configuration
manhattan :: (Eq a) => Int -> Int -> Matrix a -> (Int, a) -> Int
manhattan height width b (n,x) = abs (row n - row m) + abs (column n - column m)
        where
                m = fromJust $ elemIndex x (map snd $ content b)
                row x = x `div` width
                column x = x `mod` height

manhattan_sum' :: (Eq a) => Int -> Int -> Matrix a -> Matrix a -> Int
manhattan_sum' height width b1 b2 = fromIntegral $ sum $ map (manhattan height width b1) (content b2)

--------------
-- TESTCASE #0 - fixtures
--------------
boardHeight0 = 2
boardWidth0 = 2
blank0 = '.'
cellvals0 = "ABC."

adjacent0 :: [[Int]]
adjacent0 = [[1,2],[0,3],
         [0,3],[1,2]]

board0 = generateBoard blank0 boardHeight0 boardWidth0 cellvals0

board0a = M { blank = blank0,
            height = boardHeight0,
            width = boardWidth0,
            content =
            [(0,'A'), (1,'B'),
            (2,'.'), (3,'C')]}
board0b = M { blank = blank0,
            height = boardHeight0,
            width = boardWidth0,
            content =
            [(0,'A'),(1,'.'),
            (2,'C'), (3,'B')]}
nextBoards0 = [board0b, board0a]

board0c = M { blank = blank0,
        height = boardHeight0,
        width = boardWidth0,
        content = [(0,'A'),(1,'.'),
        (2,'C'),(3,'B')]}
board0d = M { blank = blank0,
        height = boardHeight0,
        width = boardWidth0,
        content = [(0,'.'),(1,'A'),
        (2,'C'),(3,'B')]}
board0e = M { blank = blank0,
        height = boardHeight0,
        width = boardWidth0,
        content = [(0,'C'),(1,'A'),
        (2,'.'),(3,'B')]}
board0f = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'C'),(1,'A'),
        (2,'B'),(3,'.')]}
board0g = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'C'),(1,'.'),
        (2,'B'),(3,'A')]}
board0h = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'.'),(1,'C'),
        (2,'B'),(3,'A')]}
board0i = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'C'),
        (2,'.'),(3,'A')]}
board0j = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'C'),
        (2,'A'),(3,'.')]}
board0k = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'.'),
        (2,'A'),(3,'C')]}
board0l = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'.'),(1,'B'),
        (2,'A'),(3,'C')]}
board0m = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'A'),(1,'B'),
        (2,'.'),(3,'C')]}
board0n = M { blank = blank0,height = boardHeight0,
        width = boardWidth0,
        content = [(0,'B'),(1,'A'),
        (2,'C'),(3,'.')]}

path0 = [board0, board0m, board0l, board0k, board0j, board0i, board0h, board0g, board0f, board0e, board0d, board0c]
paths0 = [path0, [board0, board0c]]
paths0rev = [[board0, board0c], path0]
--paths0rev = [[board0, board0m, board0l, board0k, board0j, board0i, board0h, board0g, board0f, board0e, board0d, board0c], [board0, board0c]]

--------------
-- TESTCASE #1 - fixtures
--------------
boardHeight1 = 4
boardWidth1 = 4
blank1 = '.'
cellvals1 = "ABCD\
         \EFGH\
         \IJKL\
         \MNO."

adjacent1 :: [[Int]]
adjacent1 = [[1,4],[0,2,5],[1,3,6],[2,7],
         [0,5,8], [1,4,6,9], [2,5,7,10], [3,6,11],
         [4,9,12], [5,8,10,13], [6,9,11,14], [7,10,15],
         [8,13], [9,12,14], [10,13,15], [11, 14]]

board1 = generateBoard blank1 boardHeight1 boardWidth1 cellvals1
swaps1 = [S {posFrom = 15, posTo = 11}, S {posFrom = 15, posTo = 14}]

board1a = M { blank = blank1,
         height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
         (8,'I'), (9,'J'), (10,'K'), (11,'.'),
         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}

board1b' = M { blank = blank1, height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'.'),
         (4,'E'), (5,'F'), (6,'G'), (7,'D'),
         (8,'I'), (9,'J'), (10,'K'), (11,'H'),
         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1b = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'E'), (5,'F'), (6,'G'), (7,'.'),
         (8,'I'), (9,'J'), (10,'K'), (11,'H'),
         (12,'M'), (13,'N'), (14,'O'), (15,'L')]}

board1c = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
         (8,'I'), (9,'J'), (10,'.'), (11,'K'),
         (12,'M'), (13, 'N'), (14,'O'), (15,'L')]}
nextBoards1a = [board1b, board1c, board1]

board1d = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'E'), (5,'.'), (6,'G'), (7,'H'),
         (8,'I'), (9,'F'), (10,'J'), (11,'K'),
         (12,'M'), (13, 'N'), (14,'O'), (15,'L')]}
board1ea = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'E'), (5,'F'), (6,'G'), (7,'H'),
         (8,'I'), (9,'.'), (10,'J'), (11,'K'),
         (12,'M'), (13, 'N'), (14,'O'), (15,'L')]}
board1eb = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'E'), (5,'G'), (6,'.'), (7,'H'),
         (8,'I'), (9,'F'), (10,'J'), (11,'K'),
         (12,'M'), (13, 'N'), (14,'O'), (15,'L')]}
board1ec = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
         (4,'.'), (5,'E'), (6,'G'), (7,'H'),
         (8,'I'), (9,'F'), (10,'J'), (11,'K'),
         (12,'M'), (13, 'N'), (14,'O'), (15,'L')]}
board1ed = M {  blank = blank1,height = boardHeight1,
         width = boardWidth1,
         content =
         [(0,'A'), (1,'.'), (2,'C'), (3,'D'),
         (4,'E'), (5,'B'), (6,'G'), (7,'H'),
         (8,'I'), (9,'F'), (10,'J'), (11,'K'),
         (12,'M'), (13, 'N'), (14,'O'), (15,'L')]}
nextBoards1b = [board1ed, board1ec, board1eb, board1ea]

board1f = M { blank = blank1,
        height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1i = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'.'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1j = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1k = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'.'), (11,'K'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1l = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'.'), (7,'G'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1m = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'.'), (3,'C'),
        (4,'E'), (5,'F'), (6,'G'), (7,'D'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
board1n = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'.'),(11,'L'),
        (12,'M'),(13,'N'),(14,'K'),(15,'O')]}
board1o = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'L'),(11,'.'),
        (12,'M'),(13,'N'),(14,'K'),(15,'O')]}
board1p = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'L'),(11,'O'),
        (12,'M'),(13,'N'),(14,'K'),(15,'.')]}
board1q = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'L'),(11,'O'),
        (12,'M'),(13,'N'),(14,'.'),(15,'K')]}
board1r = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'.'),(11,'O'),
        (12,'M'),(13,'N'),(14,'L'),(15,'K')]}
board1s = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'O'),(11,'.'),
        (12,'M'),(13,'N'),(14,'L'),(15,'K')]}
board1t = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'O'),(11,'K'),
        (12,'M'),(13,'N'),(14,'L'),(15,'.')]}
board1u = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'O'),(11,'K'),
        (12,'M'),(13,'N'),(14,'.'),(15,'L')]}
board1v = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'.'),(11,'K'),
        (12,'M'),(13,'N'),(14,'O'),(15,'L')]}
board1w = M { blank = blank1,height = 4,
        width = 4,
        content =
        [(0,'A'),(1,'B'),(2,'C'),(3,'D'),
        (4,'E'),(5,'F'),(6,'G'),(7,'H'),
        (8,'I'),(9,'J'),(10,'K'),(11,'.'),
        (12,'M'),(13,'N'),(14,'O'),(15,'L')]}
board1x = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'L'),
        (12,'M'), (13, 'N'), (14,'.'), (15,'O')]}
board1y = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'.'), (11,'L'),
        (12,'M'), (13, 'N'), (14,'K'), (15,'O')]}
board1z = M {  blank = blank1,height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'L'),
        (12,'M'), (13, 'O'), (14,'N'), (15,'.')]}
nextBoards1c = [board1f, board1x]
path1 :: ([Matrix Char], Integer)
path1 = ([board1], 1)
paths1 :: [([Matrix Char], Integer)]
paths1 = [([board1f, board1],0), ([board1x, board1],0)]
paths2 :: [([Matrix Char], Integer)]
paths2 = [([board1f, board1],2), ([board1x, board1],2)]
paths3 :: SList ([Matrix Char], Integer)
paths3 = SList [([board1t, board1],3), ([board1s, board1f, board1], 5), ([board1v,board1f,board1], 6), ([board1u, board1], 8) ]
--paths3 = SList [([board1s, board1f, board1], 5)]
paths4 :: [([Matrix Char], Integer)]
--paths4 = [([board1u, board1], 8), ([board1t, board1],3)]
paths4 = [([board1f, board1],5), ([board1x, board1],7)]
paths5 :: SList ([Matrix Char], Integer)
--paths5 = SList [([board1t, board1],3), ([board1s, board1f, board1], 5), ([board1u, board1], 8)]
paths5 = SList [([board1t, board1],3),([board1f, board1],5),([board1s, board1f, board1], 5),([board1v,board1f,board1], 6),([board1x, board1],7),([board1u, board1], 8)]
xbacklog1 = [[board1k], [board1l], [board1m],[]]
rightmost1 = [board1, board1x, board1y]

misplaced = misplaced' board1
manhattan_sum = manhattan_sum' boardHeight1 boardWidth1 board1

