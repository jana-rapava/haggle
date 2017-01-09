{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lloyd15 where

import Control.Exception (assert)
import Control.Monad (join, liftM, liftM2)
import Data.Maybe (catMaybes, fromJust)
import Data.List (find, (\\))

data Matrix a = M {
                height :: Int,
                width :: Int,
                content :: [(Int, a)]
                } deriving (Eq, Show)
data Swap a = S {
                posFrom :: Int,
                sym :: a,
                posTo :: Int
                } deriving (Eq, Show)

mkMatrix :: (Int, Int, [(Int,a)]) -> Matrix a
mkMatrix (h, w, d) = M {height = h, width = w, content = d}

generateBoard :: Int -> Int -> [a] -> Matrix a
generateBoard boardHeight boardWidth cellvals =
        let size = boardHeight * boardWidth in
                assert (length cellvals == size)
                mkMatrix $ (boardHeight, boardWidth, zip [0..size-1] cellvals)

divisible :: Int -> Int -> Bool
divisible x m = x `mod` m == 0

bool2maybe :: Bool -> a -> Maybe a
bool2maybe p x = if p then Just x else Nothing

grow :: Int -> Int -> Int -> [Int]
grow boardHeight boardWidth seed =
        let
                size = boardHeight * boardWidth
                x %- y = do
                        diff2 <- bool2maybe (diff >= 0) diff
                        bool2maybe (y /= 1 || (y == 1 && not (divisible x boardWidth))) diff2
                        where diff = x - y
                x %+ y = do
                                sum2 <- bool2maybe (sum < size) sum
                                bool2maybe (y /= 1 || (y == 1 && not (divisible sum boardWidth))) sum2
                        where sum = x + y


        in
                catMaybes [seed %- boardWidth, seed %- 1, seed %+ 1, seed %+ boardWidth]

computeAdjacent :: Int -> Int -> [[Int]]
computeAdjacent boardHeight boardWidth = [grow boardHeight boardWidth seed | seed <- [0..size-1]]
        where size = boardHeight * boardWidth

findIndex :: (a -> Bool) -> Matrix a -> Maybe Int
findIndex p b = do
                (pos, _) <- find (p . snd) (content b)
                return pos

mkSwap :: (Int, a, Int) -> Swap a
mkSwap (x,y,z) = S {posFrom = x, sym = y, posTo = z}

generateSwaps :: (Eq a) => (a -> Bool) -> Matrix a  -> [Swap a]
generateSwaps blank b = map mkSwap $ zip3 (repeat blankPos) swapSyms swapPoss
                where
                   boardHeight = height b
                   boardWidth = width b
                   blankPos = fromJust (findIndex blank b)
                   adjacent = computeAdjacent boardHeight boardWidth
                   swapPoss = adjacent !! blankPos
                   swapSyms = map snd (map ((content b) !!) swapPoss)

applySwap :: Matrix a -> Swap a -> Matrix a
applySwap board s = mkMatrix (height board, width board,
                        l3 ++ (pos1, snd x):l4 ++ (pos2, snd y):l2)
                where
                        b = content board
                        pos1 = min (posFrom s) (posTo s)
                        pos2 = max (posFrom s) (posTo s)
                        (l1,x:l2) = splitAt pos2 b
                        (l3,y:l4) = splitAt pos1 l1

nextBoards :: (Eq a) => [Matrix a] -> (a -> Bool)-> [Matrix a]
nextBoards bs p = join $ (liftM2 . liftM2) applySwap [bs] (liftM (generateSwaps p) bs)

-- this function determines the search order - change this to implement different heuristics
pick :: [Matrix a] -> (Matrix a, [Matrix a])
pick bs = (last bs, init bs)

prune :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
-- delete all items in l1 which appear in l2
prune = (\\)

generateBranch :: (Eq a) => Matrix a -> [Matrix a] -> [[Matrix a]] -> (Matrix a -> Bool) -> ([Matrix a] -> Bool) -> (a -> Bool) -> Either [[Matrix a]] ([Matrix a], [[Matrix a]])
generateBranch b path backlog stopSuccess stopFail p
        | stopFail next = Left backlog
        | stopSuccess b = Right (path, backlog)
        | otherwise = let (nextBoard, bl) = pick (prune next path) in
                generateBranch nextBoard (b:path) (bl:backlog)
                stopSuccess stopFail p
                where next = nextBoards [b] p

searchFirst :: (Eq a) => [[Matrix a]] -> (Matrix a -> Bool) -> ([Matrix a] -> Bool) -> (a -> Bool) -> [[Matrix a]] -> [[Matrix a]]
searchFirst [[]] _ _ _ paths = paths
searchFirst ([]:backlog) stopSuccess stopFail p paths = searchFirst backlog stopSuccess stopFail p paths
searchFirst ((b:bs):backlog) stopSuccess stopFail p paths = case res of
                Left endBacklog -> searchFirst endBacklog stopSuccess stopFail p paths
                Right (path, endBacklog) -> searchFirst [[]] stopSuccess stopFail p (path:paths)
        where
                res = generateBranch b [] (bs:backlog) stopSuccess stopFail p

