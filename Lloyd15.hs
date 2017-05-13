{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Lloyd15 where

import Control.Exception (assert)
import Control.Monad (join, liftM, liftM2)
import Data.Maybe (catMaybes, fromJust)
import Data.List (elemIndex, (\\), partition)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Debug.Trace

data Matrix a = M {
                blank :: a,
                height :: Int,
                width :: Int,
                content :: [(Int, a)]
                } deriving (Eq, Show)
data Swap a = S {
                posFrom :: Int,
                posTo :: Int
                } deriving (Eq, Show)

data InfInt = Fin Integer | Inf deriving (Eq, Show)

instance Num InfInt where
        (Fin x) + (Fin y) = Fin (x + y)
        Inf + _ = Inf
        _ + Inf = Inf

        (Fin x) * (Fin y) = Fin (x * y)
        Inf * _ = Inf
        _ * Inf = Inf

        abs (Fin x) = Fin (abs x)
        abs Inf = Inf

        signum (Fin x) = Fin (signum x)
        signum Inf = 1

        fromInteger x = Fin x

        negate (Fin x) = Fin (negate x)
        negate Inf = Inf


mkMatrix :: (a, Int, Int, [(Int,a)]) -> Matrix a
mkMatrix (b, h, w, d) = M {blank = b, height = h, width = w, content = d}

generateBoard :: a -> Int -> Int -> [a] -> Matrix a
generateBoard blank boardHeight boardWidth cellvals =
    let size = boardHeight * boardWidth in
        assert (length cellvals == size)
        mkMatrix $ (blank, boardHeight, boardWidth, zip [0..size-1] cellvals)

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
    in catMaybes [seed %- boardWidth, seed %- 1, seed %+ 1, seed %+ boardWidth]

computeAdjacent :: Int -> Int -> [[Int]]
computeAdjacent boardHeight boardWidth = [grow boardHeight boardWidth seed | seed <- [0..size-1]]
    where size = boardHeight * boardWidth


findBlank :: (Eq a) => Matrix a -> Maybe Int
findBlank b = elemIndex (blank b) (map snd $ content b)

mkSwap :: (Int, Int) -> Swap a
mkSwap (x,y) = S {posFrom = x,  posTo = y}

generateSwaps :: (Eq a) => Matrix a  -> [Swap a]
generateSwaps b = map mkSwap $ zip (repeat blankPos) swapPoss
    where
        boardHeight = height b
        boardWidth = width b
        blankPos = fromJust (findBlank b)
        adjacent = computeAdjacent boardHeight boardWidth
        swapPoss = adjacent !! blankPos

applySwap :: Matrix a -> Swap a -> Matrix a
applySwap board s = mkMatrix (blank board, height board, width board,
                        l3 ++ (pos1, snd x):l4 ++ (pos2, snd y):l2)
    where
        b = content board
        pos1 = min (posFrom s) (posTo s)
        pos2 = max (posFrom s) (posTo s)
        (l1,x:l2) = splitAt pos2 b
        (l3,y:l4) = splitAt pos1 l1

nextBoards :: (Eq a) => Matrix a -> [Matrix a]
nextBoards b = map (applySwap b) (generateSwaps b)

data FunctionStore a =
    FS {
        stopSuccess :: Matrix a -> Bool,
        stopFail :: [Matrix a] -> Bool,
        pick :: [Matrix a] -> (Matrix a, [Matrix a]),
        prune :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
        }

-- return from recursion until we find a branching, manipulate path and backlog accordingly
selectPromising :: (Eq a) =>
        ([Matrix a] -> [Matrix a] -> [Matrix a]) ->
        ([Matrix a] -> Bool) ->
        [[Matrix a]] ->
        [Matrix a] ->
        Maybe (Matrix a, [[Matrix a]], [Matrix a])

selectPromising _ _ [[]] _ = Nothing

-- should never happen, but compiler would complain otherwise
selectPromising _ _ ((b:bs):bss) [] = undefined

-- we are in a child node, going down the same branch
selectPromising prune stopFail ((b:bs):bss) path@(p:ps) =
   if stopFail next then
        selectPromising prune stopFail (bs:bss) ps else
        Just (b, bs:bss, ps)
   where next = prune (nextBoards b) (b:path)

selectPromising prune stopFail ([]:bss) [] =
        selectPromising prune stopFail bss []

selectPromising prune stopFail ([]:bss) paths@(p:ps) =
        selectPromising prune stopFail bss paths

dfs' :: (Eq a, Show a) =>
    Matrix a ->
    StateT ([Matrix a], [[Matrix a]]) (Reader (FunctionStore a)) [[Matrix a]]

dfs' b = do
    stopSuccess <- asks stopSuccess
    stopFail <- asks stopFail
    pick <- asks pick
    prune <- asks prune
    (path, backlog) <- get
    let
        path' = b:path
        next = prune (nextBoards b) path' in
--        next = prune (nextBoards [b] blank) (trace ("\npath': " ++ show (map content path') ++ "\nbacklog: " ++ show (((map.map) content) backlog)) path') in
        if (stopSuccess b)
        then
            case selectPromising prune stopFail backlog path' of
                Nothing -> return [path']
                Just (next2, backlog2, path2) -> do
                    put (path2, backlog2)
--                    put ((trace ("\n path2: " ++ show path2) path2), (trace ("\nbacklog2: " ++ show backlog2) backlog2))
                    paths <- dfs' next2
--                    paths <- dfs' (trace (" \n next2: " ++ show next2) next2) blank
                    return (path':paths)
        else
              if (stopFail next)
--            if (trace ("\nnext: " ++ show next) stopFail next)
            then
                case selectPromising prune stopFail backlog path' of
                    Nothing -> return []
                    Just (next2, backlog2, path2) -> do
                        put (b:path2, backlog2)
                        dfs' next2
            else
                let (nextBoard, bl) = pick next in do
                    put (path', bl:backlog)
--                    put (path', trace ("\n bl: " ++ show bl) (bl:backlog))
                    dfs' nextBoard

dfs :: (Eq a, Show a) => Matrix a -> FunctionStore a -> [[Matrix a]]
dfs b fs = fst $ runReader (runStateT (dfs' b) ([],[])) fs

merge :: (Show a) => [[Matrix a]] -> [[Matrix a]] -> [[Matrix a]]
merge [] [] = []
--merge xss ([]:yss) = undefined
merge [] yss = yss
merge (xs:xss) (ys:yss) = (zipWith (:) xs (repeat ys)) ++ (merge  xss yss)
--merge (xs:xss) (ys:yss) = (zipWith (:) xs (repeat ys)) ++ (merge (trace ("\n xss: " ++ show xss) xss) (trace ("\n yss: " ++ show yss) yss))
--merge (xs:xss) (ys:yss) = (zipWith (:) (trace ("\n xs: " ++ show xs) xs) (repeat (trace ("\n ys: " ++ show ys) ys))) ++ (merge xss yss)

bfs' :: (Eq a, Show a) =>
        [Matrix a] ->
        InfInt ->
        StateT ([[Matrix a]]) (Reader (FunctionStore a)) [[Matrix a]]
bfs' level depth = do
    stopSuccess <- asks stopSuccess
    stopFail <- asks stopFail
    pick <- asks pick
    prune <- asks prune
    paths <- get
    let
        prunedLast = filter (not . stopSuccess) level
--        (res, prunedLast) = partition stopSuccess level
--        (res, prunedLast) = partition stopSuccess (trace ("\n level: " ++ show level) level)
        next = map nextBoards prunedLast
--        next = map nextBoards (trace ("\n prunedLast: " ++ show (map content prunedLast)) prunedLast)
        prunedNext = zipWith prune next paths in
--        prunedNext = zipWith prune (trace ("\n next: " ++ show ((map.map) content next)) next) paths in
        if (depth == 0 || stopFail level)
        then do
                return []
        else
            if (any stopSuccess level)
            then
            let (fin, pathsFin) = partition (stopSuccess . head) paths in do
                put (merge prunedNext pathsFin)
--                put (merge prunedNext (trace ("\n  pathsFin: " ++ show ((map.map) content pathsFin)) pathsFin))
--                    fin2 <- bfs' (concat (trace ("\n prunedNext fin: " ++ show ((map.map) content prunedNext)) prunedNext)) (depth-1)
                fin2 <- bfs' (concat prunedNext) (depth-1)
                return (fin ++ fin2)
--                return ((trace ("\n  fin: " ++ show ((map.map) content fin)) fin) ++ fin2)
            else do
                put (merge prunedNext paths)
--                    bfs' (concat (trace ("\n prunedNext cont: " ++ show ((map.map) content prunedNext)) prunedNext)) (depth-1)
                bfs' (concat prunedNext) (depth-1)

bfs :: (Eq a, Show a) => Matrix a -> InfInt -> FunctionStore a -> [[Matrix a]]
bfs b depth fs = fst $ runReader (runStateT (bfs' [b] depth) [[b]]) fs
