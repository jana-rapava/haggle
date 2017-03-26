module TestLloyd15 where

import Test.HUnit
import Lloyd15
import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy, (\\))
import Data.Function (on)
import Control.Monad.Reader
import Control.Monad.State.Lazy

-- this function determines the search order - change this to implement different heuristics
pickBasic :: [Matrix a] -> (Matrix a, [Matrix a])
pickBasic bs = (last bs, init bs)

-- this function prunes the branches of search space
pruneBasic :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
-- delete all items in l1 which appear in l2
pruneBasic = (\\)

genPick :: (Matrix a -> Int) -> [Matrix a] -> (Matrix a, [Matrix a])
genPick rank xs = (head res, tail res)
        where
                res = map snd $ sortBy (compare `on` fst) $ zip (map rank xs) xs

-- heuristics 1: pick the board with lowest number of misplaced tiles
misplaced :: (Eq a) => Matrix a -> Matrix a -> Int
misplaced b1 b2 = length $ filter (== False) $ zipWith (==) (content b1) (content b2)

-- heuristics 2: pick the board with lowest sum of Manhattan distances from the goal configuration
manhattan :: (Eq a) => Int -> Int -> Matrix a -> (Int, a) -> Int
manhattan height width b (n,x) = abs (row n - row m) + abs (column n - column m)
        where
                m = fromJust $ findIndex (==x) b
                row x = x `div` width
                column x = x `mod` height

manhattan_sum :: (Eq a) => Int -> Int -> Matrix a -> Matrix a -> Int
manhattan_sum height width b1 b2 = sum $ map (manhattan height width b1) (content b2)

--------------
-- TESTCASE #1
--------------
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
swaps1 = [S {posFrom = 15, posTo = 11}, S {posFrom = 15, posTo = 14}]
blank1 = (=='.')
board1' = M { height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard1 = M { height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'.'),
        (4,'E'), (5,'F'), (6,'G'), (7,'D'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard2 = M { height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'.'),
        (8,'I'), (9,'J'), (10,'K'), (11,'H'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
xboard3 = M { height = boardHeight1,
        width = boardWidth1,
        content =
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')]}
nextBoards1 = [[(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'.'),
        (12,'M'), (13,'N'), (14,'O'), (15,'L')],
        [(0,'A'), (1,'B'), (2,'C'), (3,'D'),
        (4,'E'), (5,'F'), (6,'G'), (7,'H'),
        (8,'I'), (9,'J'), (10,'K'), (11,'L'),
        (12,'M'), (13,'N'), (14,'.'), (15,'O')]]
rightmost1 = [xboard3, xboard2, xboard1]
stopSuccess1 = (== board1)
stopFail1 :: [Matrix a] -> Bool
stopFail1 = null

xpick11 :: [Matrix Char] -> (Matrix Char, [Matrix Char])
xpick11 = genPick (misplaced board1)

xpick12 :: [Matrix Char] -> (Matrix Char, [Matrix Char])
xpick12 = genPick (manhattan_sum boardHeight1 boardWidth1 board1)

testGenerateBoard1 :: Test
testGenerateBoard1 = TestCase $ assertEqual ""
        [0..boardHeight1*boardWidth1-1]
        (map fst $ content $ generateBoard boardHeight1 boardWidth1 cellvals1)

testGenerateBoard2 :: Test
testGenerateBoard2 = TestCase $ assertEqual ""
        cellvals1
        (map snd $ content $ generateBoard boardHeight1 boardWidth1 cellvals1)

testComputeAdjacent1 :: Test
testComputeAdjacent1 = TestCase $ assertEqual ""
        adjacent1 (computeAdjacent boardHeight1 boardWidth1)

testGenerateSwaps1 :: Test
testGenerateSwaps1 = TestCase $ assertEqual "chars"
        swaps1 (generateSwaps blank1 board1)

testApplySwap1 :: Test
testApplySwap1 = TestCase $ assertEqual ""
        board1' (applySwap board1 (head swaps1))

testApplySwap2 :: Test
testApplySwap2 = TestCase $ assertEqual ""
        xboard2 (applySwap xboard1 (S { posFrom = 3, posTo = 7}))

testNextBoards1 :: Test
testNextBoards1 = TestCase $ assertEqual ""
        nextBoards1 (map content $ nextBoards [board1] blank1)

testGenerateBranch1 :: Test
testGenerateBranch1 = TestCase $ assertEqual ""
        rightmost1 (fst $ snd $ runReader
        (runStateT (generateBranch xboard1 blank1) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        pick = pickBasic,
        prune = pruneBasic})
        )

testGenerateBranch4 :: Test
testGenerateBranch4 = TestCase $ assertEqual ""
        rightmost1 (fst $ snd $ runReader
        (runStateT (generateBranch xboard1 blank1) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        pick = xpick11,
        prune = pruneBasic})
        )

testGenerateBranch5 :: Test
testGenerateBranch5 = TestCase $ assertEqual ""
        rightmost1 (fst $ snd $ runReader
        (runStateT (generateBranch xboard1 blank1) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess1,
        stopFail = stopFail1,
        pick = xpick11,
        prune = pruneBasic})
        )

testSearchFirst1 :: Test
testSearchFirst1 = TestCase $ assertEqual ""
       rightmost1 (fromJust $ searchFirst xboard1 blank1
       (FS {
       stopSuccess = stopSuccess1,
       stopFail = stopFail1,
       pick = pickBasic,
       prune = pruneBasic})
       )
boardHeight2 = 2
boardWidth2 = 7
cellvals2 = [1, 2, 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 0]
adjacent2 = [[1,7],[0,2,8],[1,3,9],[2,4,10],[3,5,11],[4,6,12],[5,13],
        [0,8],[1,7,9],[2,8,10],[3,9,11],[4,10,12],[5,11,13],[6,12]]
board2 = generateBoard boardHeight2 boardWidth2 cellvals2
swaps2 = [S {posFrom = 13, posTo = 6}, S {posFrom = 13, posTo = 12}]
blank2 = (==0)
board2' = M { height = boardHeight2,
        width = boardWidth2,
        content =
        [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,0),
        (7,8),(8,9), (9,10), (10,11), (11,12),(12,13), (13,7)]}
yboard1 = M { height = boardHeight2,
        width = boardWidth2,
        content =
        [(0,1), (1,2), (2,3), (3,0),(4,5), (5,6), (6,7),
        (7,8),(8,9), (9,10), (10,4), (11,11),(12,12), (13,13)]}
yboard2 = M { height = boardHeight2,
        width = boardWidth2,
        content =
        [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
        (7,8),(8,9), (9,10), (10,0), (11,11),(12,12), (13,13)]}
yboard3 = M { height = boardHeight2,
        width = boardWidth2,
        content =
        [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
        (7,8),(8,9), (9,10), (10,11), (11,0),(12,12), (13,13)]}
yboard4 = M { height = boardHeight2,
        width = boardWidth2,
        content =
        [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
        (7,8),(8,9), (9,10), (10,11), (11,12),(12,0), (13,13)]}
nextBoards2 = [[(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,0),
        (7,8),(8,9), (9,10), (10,11), (11,12),(12,13), (13,7)],
        [(0,1), (1,2), (2,3), (3,4),(4,5), (5,6), (6,7),
        (7,8),(8,9), (9,10), (10,11), (11,12),(12,0), (13,13)]]
rightmost2 = [yboard4, yboard3, yboard2, yboard1]
stopSuccess2 = (== board2)
stopFail2 :: [Matrix a] -> Bool
stopFail2 = null

xpick21 :: [Matrix Int] -> (Matrix Int, [Matrix Int])
xpick21 = genPick (misplaced board2)

xpick22 :: [Matrix Int] -> (Matrix Int, [Matrix Int])
xpick22 = genPick (manhattan_sum boardHeight2 boardWidth2 board2)

testGenerateBoard3 :: Test
testGenerateBoard3 = TestCase $ assertEqual ""
        [0..boardHeight2*boardWidth2-1]
        (map fst $ content $ generateBoard boardHeight2 boardWidth2 cellvals2)

testGenerateBoard4 :: Test
testGenerateBoard4 = TestCase $ assertEqual ""
        cellvals2
        (map snd $ content $ generateBoard boardHeight2 boardWidth2 cellvals2)

testComputeAdjacent2 :: Test
testComputeAdjacent2 = TestCase $ assertEqual ""
        adjacent2 (computeAdjacent boardHeight2 boardWidth2)

testGenerateSwaps2 :: Test
testGenerateSwaps2 = TestCase $ assertEqual "nums"
        swaps2 (generateSwaps blank2 board2 )

testApplySwap3 :: Test
testApplySwap3 = TestCase $ assertEqual ""
        board2' (applySwap board2 (head swaps2))

testNextBoards2 :: Test
testNextBoards2 = TestCase $ assertEqual ""
        nextBoards2 (map content $ nextBoards [board2] blank2)

testGenerateBranch2 :: Test
testGenerateBranch2 = TestCase $ assertEqual ""
        rightmost2 (fst $ snd $ runReader
        (runStateT (generateBranch yboard1 blank2) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess2,
        stopFail = stopFail2,
        pick = pickBasic,
        prune = pruneBasic})
        )

testGenerateBranch6 :: Test
testGenerateBranch6 = TestCase $ assertEqual ""
        rightmost2 (fst $ snd $ runReader
        (runStateT (generateBranch yboard1 blank2) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess2,
        stopFail = stopFail2,
        pick = xpick21,
        prune = pruneBasic})
        )

testGenerateBranch7 :: Test
testGenerateBranch7 = TestCase $ assertEqual ""
        rightmost2 (fst $ snd $ runReader
        (runStateT (generateBranch yboard1 blank2) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess2,
        stopFail = stopFail2,
        pick = xpick22,
        prune = pruneBasic})
        )

testSearchFirst2 :: Test
testSearchFirst2 = TestCase $ assertEqual ""
       rightmost2 (fromJust $ searchFirst yboard1 blank2
       (FS {
       stopSuccess = stopSuccess2,
       stopFail = stopFail2,
       pick = pickBasic,
       prune = pruneBasic})
       )
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
swaps3 = [S {posFrom = 14, posTo = 11}, S {posFrom = 14, posTo = 13}]
blank3 = (=="")
board3' = M { height = boardHeight3,
        width = boardWidth3,
        content =
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,"adipiscing"), (7,"elit"), (8, "sed"),
        (9,"do"), (10,"eiusmod"), (11,"") ,
        (12,"incididunt"), (13,"ut"),(14,"tempor")]}
zboard1 = M { height = boardHeight3,
        width = boardWidth3,
        content =
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,""), (4,"amet"), (5,"consectetur"),
        (6,"sit"), (7,"elit"), (8, "sed"),
        (9,"adipiscing"), (10,"eiusmod"), (11,"tempor") ,
        (12,"do"), (13,"incididunt"),(14,"ut")]}
zboard2 = M { height = boardHeight3,
        width = boardWidth3,
        content =
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,""), (7,"elit"), (8, "sed"),
        (9,"adipiscing"), (10,"eiusmod"), (11,"tempor") ,
        (12,"do"), (13,"incididunt"), (14,"ut")]}
zboard3 = M { height = boardHeight3,
        width = boardWidth3,
        content =
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,"adipiscing"), (7,"elit"), (8, "sed"),
        (9,""), (10,"eiusmod"), (11,"tempor") ,
        (12,"do"), (13,"incididunt"), (14,"ut")]}
zboard4 = M { height = boardHeight3,
        width = boardWidth3,
        content =
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,"adipiscing"), (7,"elit"), (8, "sed"),
        (9,"do"), (10,"eiusmod"), (11,"tempor") ,
        (12,""),(13,"incididunt"), (14,"ut")]}
zboard5 = M { height = boardHeight3,
        width = boardWidth3,
        content =
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,"adipiscing"), (7,"elit"), (8, "sed"),
        (9,"do"), (10,"eiusmod"), (11,"tempor") ,
        (12,"incididunt"), (13,""),(14,"ut")]}
nextBoards3 = [[(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,"adipiscing"), (7,"elit"), (8, "sed"),
        (9,"do"), (10,"eiusmod"), (11,"") ,
        (12,"incididunt"), (13,"ut"),(14,"tempor")],
        [(0,"Lorem"), (1,"ipsum"), (2,"dolor"),
        (3,"sit"), (4,"amet"), (5,"consectetur"),
        (6,"adipiscing"), (7,"elit"), (8, "sed"),
        (9,"do"), (10,"eiusmod"), (11,"tempor") ,
        (12,"incididunt"), (13,""),(14,"ut")]]
rightmost3 = [zboard5, zboard4, zboard3, zboard2, zboard1]
stopSuccess3 = (== board3)
stopFail3 :: [Matrix a] -> Bool
stopFail3 = null

xpick31 :: [Matrix String] -> (Matrix String, [Matrix String])
xpick31 = genPick (misplaced board3)

xpick32 :: [Matrix String] -> (Matrix String, [Matrix String])
xpick32 = genPick (manhattan_sum boardHeight3 boardWidth3 board3)

testGenerateBoard5 :: Test
testGenerateBoard5 = TestCase $ assertEqual ""
        [0..boardHeight3*boardWidth3-1]
        (map fst $ content $ generateBoard boardHeight3 boardWidth3 cellvals3)

testGenerateBoard6 :: Test
testGenerateBoard6 = TestCase $ assertEqual ""
        cellvals3
        (map snd $ content $ generateBoard boardHeight3 boardWidth3 cellvals3)

testComputeAdjacent3 :: Test
testComputeAdjacent3 = TestCase $ assertEqual ""
        adjacent3 (computeAdjacent boardHeight3 boardWidth3)

testGenerateSwaps3 :: Test
testGenerateSwaps3 = TestCase $ assertEqual "strings"
        swaps3 (generateSwaps blank3 board3)

testApplySwap4 :: Test
testApplySwap4 = TestCase $ assertEqual ""
        board3' (applySwap board3 (head swaps3))

testNextBoards3 :: Test
testNextBoards3 = TestCase $ assertEqual ""
        nextBoards3 (map content $ nextBoards [board3] blank3)

testGenerateBranch3 :: Test
testGenerateBranch3 = TestCase $ assertEqual ""
        rightmost3 (fst $ snd $ runReader
        (runStateT (generateBranch zboard1 blank3) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess3,
        stopFail = stopFail3,
        pick = pickBasic,
        prune = pruneBasic})
        )

testGenerateBranch8 :: Test
testGenerateBranch8 = TestCase $ assertEqual ""
        rightmost3 (fst $ snd $ runReader
        (runStateT (generateBranch zboard1 blank3) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess3,
        stopFail = stopFail3,
        pick = xpick31,
        prune = pruneBasic})
        )

testGenerateBranch9 :: Test
testGenerateBranch9 = TestCase $ assertEqual ""
        rightmost3 (fst $ snd $ runReader
        (runStateT (generateBranch zboard1 blank3) ([],[[]]))
        (FS {
        stopSuccess = stopSuccess3,
        stopFail = stopFail3,
        pick = xpick32,
        prune = pruneBasic})
        )

testSearchFirst3 :: Test
testSearchFirst3 = TestCase $ assertEqual ""
       rightmost3 (fromJust $ searchFirst zboard1 blank3
       (FS {
       stopSuccess = stopSuccess3,
       stopFail = stopFail3,
       pick = pickBasic,
       prune = pruneBasic})
       )

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
        testGenerateSwaps3,
        testApplySwap1,
        testApplySwap2,
        testApplySwap3,
        testApplySwap4,
        testNextBoards1,
        testNextBoards2,
        testNextBoards3,
        testGenerateBranch1,
        testGenerateBranch2,
        testGenerateBranch3,
        testGenerateBranch4,
        testGenerateBranch5,
        testGenerateBranch6,
        testGenerateBranch7,
        testGenerateBranch8,
        testGenerateBranch9,
        testSearchFirst1,
        testSearchFirst2,
        testSearchFirst3
         ]
