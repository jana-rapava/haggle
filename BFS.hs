module BFS where

import Lloyd15
import FunctionStore
import Data.List (partition)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Debug.Trace

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
