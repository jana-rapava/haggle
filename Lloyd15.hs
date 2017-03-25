{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Lloyd15 where

import Control.Exception (assert)
import Control.Monad (join, liftM, liftM2)
import Data.Maybe (catMaybes, fromJust)
import Data.List (find, (\\))
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

data Matrix a = M {
                height :: Int,
                width :: Int,
                content :: [(Int, a)]
                } deriving (Eq, Show)
data Swap a = S {
                posFrom :: Int,
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

mkSwap :: (Int, Int) -> Swap a
mkSwap (x,y) = S {posFrom = x,  posTo = y}

generateSwaps :: (Eq a) => (a -> Bool) -> Matrix a  -> [Swap a]
generateSwaps blank b = map mkSwap $ zip (repeat blankPos) swapPoss
                where
                   boardHeight = height b
                   boardWidth = width b
                   blankPos = fromJust (findIndex blank b)
                   adjacent = computeAdjacent boardHeight boardWidth
                   swapPoss = adjacent !! blankPos

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
nextBoards bs blank = join $ (liftM2 . liftM2) applySwap [bs] (liftM (generateSwaps blank) bs)

-- this function determines the search order - change this to implement different heuristics
pickBasic :: [Matrix a] -> (Matrix a, [Matrix a])
pickBasic bs = (last bs, init bs)

-- this function prunes the branches of search space
pruneBasic :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
-- delete all items in l1 which appear in l2
pruneBasic = (\\)

data FunctionStore a = FS {
                        stopSuccess :: Matrix a -> Bool,
                        stopFail :: [Matrix a] -> Bool,
                        pick :: [Matrix a] -> (Matrix a, [Matrix a]),
                        prune :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
                        }

generateBranch :: (Eq a) => Matrix a -> (a -> Bool) -> StateT ([Matrix a], [[Matrix a]]) (Reader (FunctionStore a)) Bool
generateBranch b blank = do
        stopSuccess <- asks stopSuccess
        stopFail <- asks stopFail
        pick <- asks pick
        prune <- asks prune
        (path, backlog) <- get
        if (stopSuccess b) then return True else
                if (stopFail next) then return False else
                        let (nextBoard, bl) = pick (prune next path) in do
                                put (b:path, bl:backlog)
                                generateBranch nextBoard blank
                where next = nextBoards [b] blank

searchFirst' :: (Eq a) => (a -> Bool) -> (Matrix a ->  Bool) -> ([Matrix a] -> Bool) -> State [[Matrix a]] (Maybe [Matrix a])
searchFirst' blank stopSuccess stopFail = do
        backlog <- get
        case backlog of
                [[]] -> return Nothing
                ([]:bs) -> do
                                put bs
                                searchFirst' blank stopSuccess stopFail
                ((b:bs):bss) -> if res then return (Just path) else do
                                put backlog1
                                searchFirst' blank stopSuccess stopFail
                        where
                                st = runReader (runStateT (generateBranch b blank) ([],bs:bss)) (FS { stopSuccess = stopSuccess, stopFail = stopFail, pick = pickBasic, prune = pruneBasic})
                                res = fst $ st
                                path = fst $ snd $ st
                                backlog1 = snd $ snd $ st

searchFirst :: (Eq a) => Matrix a -> (a -> Bool) -> (Matrix a -> Bool) -> ([Matrix a] -> Bool) -> Maybe [Matrix a]
searchFirst b blank stopSuccess stopFail = do
        fst $ runState (searchFirst' blank stopSuccess stopFail) [[b]]
