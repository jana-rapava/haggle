{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Lloyd15 where

import Control.Exception (assert)
import Control.Monad (join, liftM, liftM2)
import Data.Function (on)
import Data.Maybe (catMaybes, fromJust)
import Data.List (elemIndex, (\\), partition, sortBy)
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
--        pick :: [Matrix a] -> (Matrix a, [Matrix a]),
        rank :: Matrix a -> Int,
        prune :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
        }

-- return from recursion until we find a branching, manipulate path and backlog accordingly
selectPromising :: (Eq a, Show a) =>
        ([Matrix a] -> [Matrix a] -> [Matrix a]) ->
        (Matrix a -> Bool) ->
        ([Matrix a] -> Bool) ->
        [[Matrix a]] ->
        [Matrix a] ->
        Maybe (Matrix a, [[Matrix a]], [Matrix a])

selectPromising _ _ _ [[]] _ = Nothing

-- should never happen, but compiler would complain otherwise
selectPromising _ _ _ ((b:bs):bss) [] = undefined

-- we are in a child node, going down the same branch
selectPromising prune stopSuccess stopFail ((b:bs):bss) path@(p:ps) =
   if stopSuccess b
   then
        Just (b, bs:bss, ps)
   else
        if stopFail next
        then
            selectPromising prune stopSuccess stopFail (bs:bss) ps
        else
            Just (b, bs:bss, ps)
   where next = prune (nextBoards b) (b:path)

selectPromising prune stopSuccess stopFail ([]:bss) [] =
        selectPromising prune stopSuccess stopFail bss []

selectPromising prune stopSuccess stopFail ([]:bss) paths@(p:ps) =
        selectPromising prune stopSuccess stopFail bss ps

genPick :: (Matrix a -> Int) -> [Matrix a] -> (Matrix a, [Matrix a])
genPick rank xs = (head res, tail res)
        where
                res = map snd $ sortBy (compare `on` fst) $ zip (map rank xs) xs

dfs' :: (Eq a, Show a) =>
    Matrix a ->
    StateT ([Matrix a], [[Matrix a]]) (Reader (FunctionStore a)) [[Matrix a]]

dfs' b = do
    stopSuccess <- asks stopSuccess
    stopFail <- asks stopFail
    rank <- asks rank
    prune <- asks prune
    (path, backlog) <- get
    let
        path' = b:path
        pick = genPick rank
        next = prune (nextBoards b) path' in
--        next = prune (nextBoards [b] blank) (trace ("\npath': " ++ show (map content path') ++ "\nbacklog: " ++ show (((map.map) content) backlog)) path') in
        if (stopSuccess b)
        then
            case selectPromising prune stopSuccess stopFail backlog path' of
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
                case selectPromising prune stopSuccess stopFail backlog path' of
                    Nothing -> return []
                    Just (next2, backlog2, path2) -> do
                        put (b:path2, backlog2)
                        dfs' next2
            else
                let (nextBoard, bl) = pick next in do
                    put (path', bl:backlog)
--                    put (path', trace ("\n bl: " ++ show bl ++ "\n backlog: " ++ show (backlog)) (bl:backlog))
                    dfs' nextBoard

dfs :: (Eq a, Show a) => Matrix a -> FunctionStore a -> [[Matrix a]]
dfs b fs = fst $ runReader (runStateT (dfs' b) ([],[])) fs

type Path a = ([Matrix a], Int)

newtype SortedList a = SortedList {getSortedList :: [a]} deriving (Functor, Show, Eq)

split :: [Path a] -> Maybe (Path a, [Path a])
split = undefined

pickAndMerge :: (Matrix a -> Int) -> [Matrix a] -> [Path a] -> [Path a]
pickAndMerge = undefined

addTo :: [Path a] -> [Path a] -> [Path a]
addTo = undefined

befs' :: (Eq a, Show a) =>
        Path a ->
        StateT (SortedList (Path a)) (Reader (FunctionStore a)) [[Matrix a]]
befs' path = do
        stopSuccess <- asks stopSuccess
        stopFail <- asks stopFail
        rank <- asks rank
        prune <- asks prune
        backlog' <- get
        let
            b = (head . fst) path
            backlog = getSortedList backlog'
            next = prune (nextBoards b) (fst path) in
--          next = prune (nextBoards [b] blank) (trace ("\npath': " ++ show (map content path') ++ "\nbacklog: " ++ show (((map.map) content) backlog)) path') in
            if (stopSuccess b)
            then
                case split backlog of
                Nothing -> return [fst path]
                Just (path2, backlog2) -> do
                    put (SortedList backlog2)
--                  put ((trace ("\n path2: " ++ show path2) path2), (trace ("\nbacklog2: " ++ show backlog2) backlog2))
                    paths <- befs' path2
--                    paths <- dfs' (trace (" \n next2: " ++ show next2) next2) blank
                    return ((fst path):paths)
            else
                if (stopFail next)
--              if (trace ("\nnext: " ++ show next) stopFail next)
                then
                    case split backlog of
                    Nothing -> return []
                    Just (path2, backlog2) -> do
                        put (SortedList backlog2)
                        befs' path2
                else
                    let paths2 = pickAndMerge rank next backlog
                        backlog2 = addTo backlog paths2 in do
--                    put (path', trace ("\n bl: " ++ show bl ++ "\n backlog: " ++ show (backlog)) (bl:backlog))
                        put (SortedList $ tail backlog2)
                        befs' (head backlog2)


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
    rank <- asks rank
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

comb :: Int -> [a] -> [[a]]
comb k []
        | k == 0 = [[]]
        | otherwise = []
comb k (x:xs)
        | k == 0 = [[]]
        | otherwise = (map (x:) (comb (k-1) xs)) ++ (comb k xs)

-- while we have nonzero number of available discrepancies, run as DFS
-- if we reach 0, return Nothing
selectNodes :: (Eq a, Show a) =>
        ([Matrix a] -> [Matrix a] -> [Matrix a]) ->
        (Matrix a -> Bool) ->
        ([Matrix a] -> Bool) ->
        [[Matrix a]] ->
        [Matrix a] ->
        Int ->
        Maybe (Matrix a, [[Matrix a]], [Matrix a])

selectNodes _ _ _ _ _ 0 = Nothing

selectNodes _ _ _ [[]] _ _ = Nothing

selectNodes prune stopSuccess stopFail ((b:bs):bss) [] _ = undefined

selectNodes prune stopSuccess stopFail ((b:bs):bss) path@(p:ps) d =
   if stopSuccess b
   then
        Just (b, bs:bss, ps)
   else
        if stopFail next
        then
            selectNodes prune stopSuccess stopFail (bs:bss) ps (d-1)
        else
            Just (b, bs:bss, ps)
   where next = prune (nextBoards b) (b:path)

selectNodes prune stopSuccess stopFail ([]:bss) [] d =
        selectNodes prune stopSuccess stopFail bss [] d

selectNodes prune stopSuccess stopFail ([]:bss) path@(p:ps) d =
        selectNodes prune stopSuccess stopFail bss ps d

-- LDS: iteratively restart search, where you don't call pick in 1,2,..,d points
-- 3 urovne: iteracia cez 1,2,..,d
-- iteracia cez mozne uzly bez heuristiky
-- samotne prehladavanie
-- hladaj len jedno riesenie??
-- ako rozumne ukladat stav? co je pre nas relevantne, aby sme vedeli dobre pokracovat? dava zmysel povedat, ze lds musi byt v postupnosti prehladavani vzdy posledne?
lds' :: (Eq a, Show a) =>
        Matrix a ->
        Int ->
        StateT ([Matrix a],[[Matrix a]]) (Reader (FunctionStore a)) [[Matrix a]]
lds' b d = do
    stopSuccess <- asks stopSuccess
    stopFail <- asks stopFail
    rank <- asks rank
    prune <- asks prune
    (path, backlog) <- get
    let
        path' = b:path
        pick = genPick rank
        next = prune (nextBoards b) path' in
--        next = prune (nextBoards [b] blank) (trace ("\npath': " ++ show (map content path') ++ "\nbacklog: " ++ show (((map.map) content) backlog)) path') in
        if (stopSuccess b)
        then
            case selectNodes prune stopSuccess stopFail backlog path' d of
                Nothing -> return [path']
                Just (next2, backlog2, path2) -> do
                    put (path2, backlog2)
--                    put ((trace ("\n path2: " ++ show path2) path2), (trace ("\nbacklog2: " ++ show backlog2) backlog2))
                    paths <- lds' next2 d
--                    paths <- lds' (trace (" \n next2: " ++ show next2) next2) blank
                    return (path':paths)
        else
              if (stopFail next)
--            if (trace ("\nnext: " ++ show next) stopFail next)
            then
                case selectNodes prune stopSuccess stopFail backlog path' d of
                    Nothing -> return []
                    Just (next2, backlog2, path2) -> do
                        put (b:path2, backlog2)
                        lds' next2 d
            else
                let (nextBoard, bl) = pick next in do
                    put (path', bl:backlog)
--                    put (path', trace ("\n bl: " ++ show bl) (bl:backlog))
                    lds' nextBoard (d-1)

lds :: (Eq a, Show a) => Matrix a -> Int -> FunctionStore a -> [[Matrix a]]
lds b dis fs = concat $ [fst $ runReader (runStateT (lds' b d) ([],[])) fs | d <- [0..dis]]
